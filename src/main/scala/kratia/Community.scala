package kratia

import java.util.UUID

import cats.{Applicative, Monad, MonadError}
import cats.implicits._
import cats.effect.Sync
import fs2.async.mutable.Topic
import kratia.state.{State, Store}
import kratia.utils.CaseEnum
import kratia.Community._
import kratia.Collector._
import org.http4s.Status

trait Community[F[_]] { self =>

  def name: String

  def domain: String

  def members: Store[F, Membership]

  def active: Store[F, Collector[F]]

  def allocation: State[F, Map[DecisionType, InfluenceDistributionType]]

  def resolution: State[F, Map[DecisionType, DecisionResolutionType]]

  def events: Topic[F, CommunityEvents]

  def collectorConstructor(decision: Decision, community: Community[F], decisionAction: DecisionAction[F]): F[Collector[F]]

  def vote(sender: Member, address: Address, vote: Vote)(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- authorize(sender)
      collector <- active.get(address.value)(CollectorNotFound(address))
      proof <- collector.model.vote(sender, vote)
      _ <- events.publish1(CommunityEvents.Voted(vote, collector.model.decision))
    } yield proof

  def decide(sender: Member, request: DecisionRequest)(implicit F: Monad[F]): F[Address] =
    authorize(sender) *>
      collectorConstructor(request.toDecision, self, resolveAction(request)) map
      (_.address)

  def reportDecision(collector: Collector[F])(implicit F: Monad[F]): F[Unit] =
    for {
      ballot <- collector.ballot.get
      (decision, address, decisionAction) = (collector.decision, collector.address, collector.decisionAction)
      result <- resolve(decision, ballot)
      _ <- decisionAction(result)
      _ <- events.publish1(CommunityEvents.Resolved(address, decision, result))
    } yield ()

  private def authorize(member: Member)(implicit F: MonadError[F, Throwable]): F[Unit] =
    members.exists(member.membership) >>= (exists => if (exists) F.unit else F.raiseError(UnauthorizedMember(member)))

  private def resolve(decision: Decision, ballot: Ballot)(implicit F: Monad[F]): F[Vote] =
    for {
      allocationType <- allocation.get map (_(decision.decisionType))
      resolutionType <- resolution.get map (_(decision.decisionType))
      buckets <- influenceBuckets(ballot, allocationType)
      vote <- resolveVote(buckets, resolutionType)
    } yield vote

  private def influenceBuckets(ballot: Ballot, allocationType: InfluenceDistributionType): F[Map[Vote, Influence]] = ???

  private def resolveVote(buckets: Map[Vote, Influence], resolutionType: DecisionResolutionType): F[Vote] = ???

  private def resolveAction(decision: DecisionRequest)(implicit F: Applicative[F]): DecisionAction[F] = {
    case Vote.DecisionSystemChangeVote(decisionType, distribution, resolution) =>
    case Vote.AddMemberVote(toAdd) =>
      if(toAdd) members.create()
  }
}

object Community extends CommunityTypes with CommunityFailures

private[kratia] trait CommunityTypes {

  type DecisionAction[F[_]] = Vote => F[Unit]

  case class Member(nickname: String, reputation: Int, secret: Secret, membership: Membership)

  case class Secret(value: UUID)

  case class Membership(value: UUID)

  case class Influence(value: Long)

  case class Decision(decisionType: DecisionType, description: String, domain: String)

  sealed abstract class DecisionRequest(val decisionType: DecisionType) {
    val description: String
    val domain: String
    def toDecision: Decision = Decision(decisionType, description, domain)
  }
  object DecisionRequest {
    case class DecisionSystemChangeRequest(description: String, domain: String)
      extends DecisionRequest(DecisionType.DecisionSystemChange)
    case class AddMemberRequest(description: String, domain: String, member: Member)
      extends DecisionRequest(DecisionType.AddMember)
  }

  sealed abstract class Vote(val proposal: DecisionType)
  object Vote {
    case class DecisionSystemChangeVote(
        decisionType: DecisionType,
        distribution: InfluenceDistributionType,
        resolution: DecisionResolutionType
    ) extends Vote(DecisionType.DecisionSystemChange)
    case class AddMemberVote(vote: Boolean)
      extends Vote(DecisionType.AddMember)
  }

  sealed abstract class DecisionType(val typeName: String)
  object DecisionType {
    case object DecisionSystemChange extends DecisionType("decision_system_change")
    case object AddMember            extends DecisionType("add_member")
  }

  abstract class DecisionResolutionType(val name: String)
  object DecisionResolutionType {
    case object Majority extends DecisionResolutionType("majority")
    case object LowInertia extends DecisionResolutionType("low_inertia")
    case object AtLeastOne extends DecisionResolutionType("at_least_one")
    case object Unanimous extends DecisionResolutionType("anonimous")

    implicit object EnumInstance extends CaseEnum[DecisionResolutionType] {
      override def show(a: DecisionResolutionType): String = a.name
      override def lift(s: String): Option[DecisionResolutionType] = all.find(_.name.equalsIgnoreCase(s))
      override def default: DecisionResolutionType = Majority
      override def all: Set[DecisionResolutionType] = Set(
        Majority, LowInertia, AtLeastOne, Unanimous
      )
    }
  }

  abstract class InfluenceDistributionType(val name: String)
  object InfluenceDistributionType {
    case object Autocratic extends InfluenceDistributionType("autocratic")
    case object Oligarchic extends InfluenceDistributionType("oligarchic")
    case object Meritocratic extends InfluenceDistributionType("meritocratic")
    case object Democratic extends InfluenceDistributionType("democratic")
    case object TemporalDegradation extends InfluenceDistributionType("temporal_degradation")
    case object GeolocationBased extends InfluenceDistributionType("geolocation_based")

    implicit object EnumInstance extends CaseEnum[InfluenceDistributionType] {
      override def show(a: InfluenceDistributionType): String = a.name
      override def lift(s: String): Option[InfluenceDistributionType] = all.find(_.name.equalsIgnoreCase(s))
      override def default: InfluenceDistributionType = Democratic
      override def all: Set[InfluenceDistributionType] = Set(
        Autocratic, Oligarchic, Meritocratic, Democratic, TemporalDegradation
      )
    }
  }

  sealed trait CommunityEvents
  object CommunityEvents {
    case class Voted(vote: Vote, decision: Decision) extends CommunityEvents
    case class Resolved(address: Address, decision: Decision, vote: Vote) extends CommunityEvents
  }
}

private[kratia] trait CommunityFailures {

  case class UnauthorizedMember(member: Member) extends RuntimeException with KratiaFailure {
    override def code: Status = Status.Unauthorized
    override def message: String = s"Operation cannot be performed for member ${member.nickname}"
  }
}
