package lambdaone.kratia.collector

import cats.MonadError
import cats.effect.Clock
import cats.implicits._
import lambdaone.kratia.collector.CollectorCRUD.CollectorFailure.{BallotBoxIsClosed, NoSuchBallotBox}
import lambdaone.kratia.collector.CollectorCRUD.BoxData
import lambdaone.toolbox.{CRUDPick, UniqueGen}

import scala.concurrent.duration._

object CollectorCRUD {

  /** Each member address -> proof of vote with the vote influence distribution */
  type AllVotes[A, P] = Map[A, (A, InfluenceAllocation[P])]

  /** The valid ballot for this box, the date to be closed (on seconds since the epoch) and all acc votes */
  case class BoxData[A, P](validBallot: Ballot[P], closedOn: Timestamp, votes: AllVotes[A, P])

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

case class CollectorCRUD[F[_], A, P](
    clock: Clock[F],
    store: CRUDPick[F, A, BoxData[A, P]],
    uniqueGen: UniqueGen[F, A]
  )(implicit F: MonadError[F, Throwable]) extends Collector[F, A, P] {

  private def fetchData(address: A): F[BoxData[A, P]] =
    store.get(address).flatMap[BoxData[A, P]] {
      case Some(data) => F.pure(data)
      case None => F.raiseError(NoSuchBallotBox(address))
    }

  private def ensureIsOpen(address: A, data: BoxData[A, P]): F[Unit] =
    clock.realTime(SECONDS).ensure(BallotBoxIsClosed(address))(_ < data.closedOn).void

  override def create(ballot: Ballot[P], closesOn: Timestamp): F[BallotBox[A, P]] =
    for {
      address <- uniqueGen.gen
      _ <- store.create(BoxData(ballot, closesOn, Map.empty), address)
    } yield BallotBox(address)

  /** Add or change the vote of a member if the box is open */
  override def vote(ballotBox: BallotBox[A, P], vote: Vote[A, P]): F[ProofOfVote[A]] = {
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

  override def validateVote(ballotBox: BallotBox[A, P], proofOfVote: ProofOfVote[A]): F[Boolean] = {
    val address = ballotBox.address
    for {
      data <- store.get(address).flatMap[BoxData[A, P]] {
        case Some(data) => F.pure(data)
        case None => F.raiseError(NoSuchBallotBox(address))
      }
      contained = data.votes.get(proofOfVote.member).fold(false)(_._1 == proofOfVote.proof)
    } yield contained
  }

  override def inspect(ballotBox: BallotBox[A, P]): F[InfluenceAllocation[P]] =
    fetchData(ballotBox.address).map(_.votes.values.toList.map(_._2).combineAll)

}


