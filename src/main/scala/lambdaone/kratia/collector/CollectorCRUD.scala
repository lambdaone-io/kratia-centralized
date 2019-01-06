package lambdaone.kratia.collector

import java.util.UUID

import cats.MonadError
import cats.effect.Clock
import cats.implicits._
import lambdaone.kratia.collector.CollectorCRUD.CollectorFailure.{BallotBoxIsClosed, NoSuchBallotBox}
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.kratia.registry.Member
import lambdaone.toolbox.{CRUDPick, UniqueGen}

import scala.concurrent.duration._

object CollectorCRUD {

  /** Each member address -> proof of vote with the vote influence distribution */
  type AllVotes = Map[Member, (UUID, InfluenceAllocation)]

  /** The valid ballot for this box, the date to be closed (on seconds since the epoch) and all acc votes */
  case class BoxData(validBallot: Ballot, closedOn: Timestamp, data: DecisionData, votes: AllVotes)

  sealed trait CollectorFailure extends RuntimeException

  object CollectorFailure {

    case class NoSuchBallotBox[A](address: A) extends CollectorFailure {

      override def getMessage: String = s"BallotBox with address $address does not exist"
    }

    case class BallotBoxIsClosed[A](address: A) extends CollectorFailure {

      override def getMessage: String = s"BallotBox with address $address is already closed"
    }

  }
}

case class CollectorCRUD[F[_]](
    clock: Clock[F],
    store: CRUDPick[F, UUID, BoxData],
    uniqueGen: UniqueGen[F, UUID]
  )(implicit F: MonadError[F, Throwable]) extends Collector[F] {

  private def fetchData(address: UUID): F[BoxData] =
    store.get(address).flatMap[BoxData] {
      case Some(data) => F.pure(data)
      case None => F.raiseError(NoSuchBallotBox(address))
    }

  private def ensureIsOpen(address: UUID, data: BoxData): F[Unit] =
    clock.realTime(SECONDS).ensure(BallotBoxIsClosed(address))(_ < data.closedOn).void

  override def create(ballot: Ballot, closesOn: Timestamp, data: DecisionData): F[BallotBox] =
    for {
      address <- uniqueGen.gen
      _ <- store.create(BoxData(ballot, closesOn, data, Map.empty), address)
    } yield BallotBox(address)

  /** Add or change the vote of a member if the box is open */
  override def vote(ballotBox: BallotBox, vote: Vote): F[ProofOfVote] = {
    val address = ballotBox.address
    for {
      data <- fetchData(address)
      _ <- ensureIsOpen(address, data)
      proof <- uniqueGen.gen
      _ <- store.update(address) { data =>
        data.copy(votes = data.votes + (vote.member -> (proof, vote.influenceAllocation)))
      }
    } yield ProofOfVote(proof, vote.member)
  }

  override def validateVote(ballotBox: BallotBox, proofOfVote: ProofOfVote): F[Boolean] = {
    val address = ballotBox.address
    for {
      data <- store.get(address).flatMap[BoxData] {
        case Some(data) => F.pure(data)
        case None => F.raiseError(NoSuchBallotBox(address))
      }
      contained = data.votes.get(proofOfVote.member).fold(false)(_._1 == proofOfVote.proof)
    } yield contained
  }

  override def inspect(ballotBox: BallotBox): F[InfluenceAllocation] =
    fetchData(ballotBox.address).map(_.votes.values.toList.map(_._2).combineAll)

  override def listOpen: F[List[BallotMetadata]] =
    for {
      now <- clock.realTime(SECONDS)
      data <- store.all
    } yield data
      .toList
      .map { case (address, data0) =>
        BallotMetadata(BallotBox(address), data0.validBallot, data0.closedOn, data0.data)
      }
      .filter { data0 =>
        data0.closesOn > now
      }
}


