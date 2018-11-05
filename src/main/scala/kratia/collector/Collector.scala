package kratia.collector

import cats.Functor
import cats.data.NonEmptyList
import cats.effect.Sync
import kratia.collector.Collector.DecisionType.Majority
import kratia.collector.Collector._
import kratia.members.Member
import kratia.utils.Address

trait Collector[F[_], P] {

  def create(ballot: Ballot[P]): F[BallotBox[P]]

  def vote(member: Member, vote: Vote)(implicit F: Sync[F]): F[ProofOfVote]

  def validateVote(proofOfVote: ProofOfVote)(implicit F: Functor[F]): F[Boolean]

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

  abstract class DecisionType
  object DecisionType {
    object Majority  extends DecisionType
    case class LowInertiaResolution(threshold: Double) extends DecisionType
  }
}

// The code below this line does not belong here
trait DecisionResolution[P,M] {
  def resolve(influenceAllocation: InfluenceAllocation[P], influence: Influence, dt: DecisionType) : List[P]

}

object DecisionResolutionInstances {
  def majorityDecisionResolution[P] = new DecisionResolution[P, Majority.type ] {
    override def resolve(influenceAllocation: InfluenceAllocation[P], influence: Influence, dt: DecisionType): List[P] = ???
  }
}
