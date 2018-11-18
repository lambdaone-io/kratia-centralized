package lambdaone.collector

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import lambdaone.collector.Collector._
import lambdaone.collector.CollectorEvent.Voted
import lambdaone.toolbox.{CRUDStore, EventStore, UniqueGen}

trait Collector[F[_], Address, P] {

  def create(ballot: Ballot[P], nickname: String): F[BallotBox[Address, P]]

  def vote(ballotBox: BallotBox[Address, P], vote: Vote[Address, P]): F[ProofOfVote[Address]]

  def validateVote(ballotBox: BallotBox[Address, P], proofOfVote: ProofOfVote[Address]): F[Boolean]

  def inspect(ballotBox: BallotBox[Address, P]): F[InfluenceAllocation[P]]

}

object Collector {

  type Influence = Double

  type InfluenceAllocation[P] = Map[P, Influence]

  sealed class BinaryProposal


  object BinaryProposal {

    object Yes extends BinaryProposal

    object No extends BinaryProposal

    val AllChoices = NonEmptyList.fromListUnsafe(List(Yes, No))

    val ballot = Ballot[BinaryProposal](BinaryProposal.AllChoices)

  }

  case class Vote[Address, P](ballot: Ballot[P], memberAddresss: Address, influenceAllocation: InfluenceAllocation[P])

  case class Ballot[P](p: NonEmptyList[P]) extends AnyVal

  case class BallotBox[Address, P](address: Address, nickname: String)

  case class ProofOfVote[Address](ref: Address, memberAddress: Address)

}

object CollectorCQRS {

  implicit def apply[F[_] : Monad, A, P](implicit
                                         event: EventStore[F, CollectorEvent[A, P]],
                                         queryVotes: CRUDStore[F, A, (A, Vote[A, P])],
                                         queryProofs: CRUDStore[F, A, (A, ProofOfVote[A])],
                                         uniqueGen: UniqueGen[F, A]
                                        ): CollectorCQRS[F, A, P]
  = new CollectorCQRS(event, queryVotes, queryProofs, uniqueGen)
}

class CollectorCQRS[F[_] : Monad, A, P](
                                         event: EventStore[F, CollectorEvent[A, P]],
                                         queryVotes: CRUDStore[F, A, (A, Vote[A, P])],
                                         queryProofs: CRUDStore[F, A, (A, ProofOfVote[A])],
                                         uniqueGen: UniqueGen[F, A]
                                       ) extends Collector[F, A, P] {

  override def create(ballot: Ballot[P], nickname: String): F[BallotBox[A, P]] =
    for {
      address <- uniqueGen.gen
      _ <- event.emit(CollectorEvent.CreatedBallotBox(address, ballot))
    } yield BallotBox(address, nickname)

  override def vote(ballotBox: BallotBox[A, P], vote: Vote[A, P]): F[ProofOfVote[A]] = {
    for {
      proof <- uniqueGen.gen
      _ <- event.emit(Voted(proof, ballotBox, vote))
    } yield ProofOfVote(proof, vote.memberAddresss)
  }

  override def validateVote(ballotBox: BallotBox[A, P], proofOfVote: ProofOfVote[A]): F[Boolean] = {
    queryProofs.get(proofOfVote.ref).map(_.map(_._2.memberAddress == proofOfVote.memberAddress).getOrElse(false))
  }

  override def inspect(ballotBox: BallotBox[A, P]): F[InfluenceAllocation[P]] =
    queryVotes.filter {
      case (ballotBoxRef, Vote(_, _, _)) => ballotBoxRef == ballotBox.address
    }.map {
      _.map { case (_, Vote(_, _, influenceAllocation)) => influenceAllocation }
    }.map(_.combineAll)
}




