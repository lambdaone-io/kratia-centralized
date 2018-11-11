package kratia.collector

import cats.Monad
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.Sync
import kratia.collector.Collector.{Vote, _}
import kratia.collector.CollectorEvent.Voted
import kratia.collector.DecisionType.Majority
import kratia.members.Member
import kratia.utils.Address
import lambdaone.toolbox.EventStore

trait Collector[F[_], P] {

  def create(ballot: Ballot[P], nickname: String)(implicit F: Sync[F]): F[BallotBox[P]]

  def vote(member: Member, vote: Vote[P])(implicit F: Sync[F]): F[ProofOfVote]

  def validateVote(proofOfVote: ProofOfVote)(implicit F: Sync[F]): F[Boolean]

  def inspect(ballotBox: BallotBox[P]): F[InfluenceAllocation[P]]

}

object Collector {

  type Influence = Double

  type InfluenceAllocation[P] = Map[P, Influence]

  sealed class BinaryProposal

  object BinaryProposal {

    object Yes extends BinaryProposal

    object No extends BinaryProposal

    val all = NonEmptyList.fromListUnsafe(List(Yes, No))

  }

  case class Vote[P](ballot: Ballot[P], m: Member, influenceAllocation: InfluenceAllocation[P])

  case class Ballot[P](p: NonEmptyList[P]) extends AnyVal

  def binaryBallot = Ballot[BinaryProposal](BinaryProposal.all)

  case class BallotBox[P](address: Address, nickname: String)

  case class ProofOfVote(ref: Address, member: Member)

}

object CollectorWithEventStore {
  implicit def CollectorWitEventStore[F[_], P](implicit store: EventStore[F, CollectorEvent[P]], F: Sync[F], M: Monad[F]): Collector[F, P] =
    new Collector[F, P]() {

      override def create(ballot: Ballot[P], nickname: String)(implicit F: Sync[F]): F[BallotBox[P]] =
        for {
          uuid <- Address.gen
          ballotBox <- store.append(CollectorEvent.CreateBallotBox(uuid, ballot))
        } yield BallotBox(uuid, nickname)

      override def vote(member: Member, vote: Vote[P])(implicit F: Sync[F]): F[ProofOfVote] = {
        for {
          ref <- Address.gen
          proof <- store.append(Voted(ref, member, vote))
        } yield ProofOfVote(ref, member)
      }

      override def validateVote(proofOfVote: ProofOfVote)(F: Sync[F]): F[Boolean] = {
        store.find {
          case e: Voted[P] => e.ref == proofOfVote.ref
        }.map(_.isDefined)
      }

      override def inspect(ballotBox: BallotBox[P]): F[InfluenceAllocation[P]] = {

      }
    }

}

abstract class CollectorEvent[P]

object CollectorEvent {

  case class CreateBallotBox[P](ref: Address, ballot: Ballot[P]) extends CollectorEvent[P]

  case class Voted[P](ref: Address, member: Member, vote: Vote[P]) extends CollectorEvent[P]

}

// The code below this line does not belong here
trait DecisionResolution[P, M] {
  def resolve(influenceAllocation: InfluenceAllocation[P], influence: Influence, dt: DecisionType): List[P]

}

object DecisionResolutionInstances {
  def majorityDecisionResolution[P] = new DecisionResolution[P, Majority.type] {
    override def resolve(influenceAllocation: InfluenceAllocation[P], influence: Influence, dt: DecisionType): List[P] = ???
  }
}


abstract class DecisionType

object DecisionType {

  object Majority extends DecisionType

  case class LowInertiaResolution(threshold: Double) extends DecisionType

}
