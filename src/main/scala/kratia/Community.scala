package kratia

import java.util.UUID

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import fs2.async.mutable.Topic
import kratia.state.{State, Store}
import kratia.utils.CaseEnum
import kratia.Collector._
import org.http4s.Status

object Community {


  /** Models */

  case class Community[F[_]](
    address: Address,
    name: String,
    domain: String,
    events: Topic[F, CommunityEvents],
    store: CommunityStore[F],
    dependencies: CommunityDependencies[F]
  )

  case class CommunityStore[F[_]](
    members: Store[F, Address],
    active: Store[F, Collector[F]],
    allocation: State[F, Map[DecisionType, InfluenceDistributionType]],
    resolution: State[F, Map[DecisionType, DecisionResolutionType]]
  )

  case class CommunityDependencies[F[_]](
    newCollector: (Decision, Community[F], DecisionAction[F]) => F[Collector[F]]
  )

  type DecisionAction[F[_]] = (Community[F], Vote) => F[Unit]

  case class Address(value: UUID) extends AnyVal

  case class Member(address: Address, nickname: String, reputation: Int, secret: Secret)

  case class Secret(value: UUID) extends AnyVal

  case class Influence(value: Long) extends AnyVal

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

  sealed abstract class Vote(val decisionType: DecisionType)

  object Vote {

    case class DecisionSystemChangeVote(
      of: DecisionType,
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

  sealed abstract class DecisionResolutionType(val name: String)

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

  sealed abstract class InfluenceDistributionType(val name: String)

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


  /** Events */

  sealed trait CommunityEvents

  object CommunityEvents {

    case class Voted(vote: Vote, decision: Decision) extends CommunityEvents

    case class Resolved(ballot: Address, decision: Decision, vote: Vote) extends CommunityEvents

    case class MemberAdded(member: Address, nickname: String) extends CommunityEvents

    case class DecisionSystemChanged(community: Address, decisionType: DecisionType, allocation: InfluenceDistributionType, resolution: DecisionResolutionType) extends CommunityEvents
  }


  /** Failures */

  case class UnauthorizedMember(member: Member) extends RuntimeException with KratiaFailure {
    override def code: Status = Status.Unauthorized
    override def message: String = s"Operation cannot be performed for member ${member.nickname}"
  }

  case class UnequalDecisionTypes(a: DecisionType, b: DecisionType) extends IllegalStateException with KratiaFailure {
    override def code: Status = Status.InternalServerError
    override def message: String = s"$a =/= $b this state should never be reached"
  }


  /** Functions */

  def vote[F[_]](sender: Member, address: Address, vote: Vote)(community: Community[F])(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- authorize(sender)
      collector <- community.store.active.get(address.value)(CollectorNotFound(address))
      proof <- Collector.vote(sender, vote)(collector.model)
      _ <- community.events.publish1(CommunityEvents.Voted(vote, collector.model.decision))
    } yield proof

  def decide[F[_]](sender: Member, request: DecisionRequest)(community: Community[F])(implicit F: Sync[F]): F[Address] =
    authorize(sender)(community)
      .*>(community.dependencies.newCollector(request.toDecision, community, resolveAction(request)))
      .map(_.address)

  def reportDecision[F[_]](collector: Collector[F])(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    for {
      ballot <- collector.store.ballot.get
      (decision, address, action) = (collector.decision, collector.address, collector.action)
      result <- resolve(decision, ballot)
      _ <- action(community, result)
      _ <- community.events.publish1(CommunityEvents.Resolved(address, decision, result))
    } yield ()

  private def authorize[F[_]](member: Member)(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    community.store.members.exists(member.address) >>= (exists => if (exists) F.unit else F.raiseError(UnauthorizedMember(member)))

  private def resolve[F[_]](decision: Decision, ballot: Ballot)(community: Community[F])(implicit F: Sync[F]): F[Vote] =
    for {
      allocationType <- community.store.allocation.get map (_(decision.decisionType))
      resolutionType <- community.store.resolution.get map (_(decision.decisionType))
      buckets <- influenceBuckets(ballot, allocationType)
      vote <- resolveVote(buckets, resolutionType)
    } yield vote

  private def influenceBuckets[F[_]](ballot: Ballot, allocationType: InfluenceDistributionType): F[Map[Vote, Influence]] = ???

  private def resolveVote[F[_]](buckets: Map[Vote, Influence], resolutionType: DecisionResolutionType): F[Vote] = ???

  private def resolveAction[F[_]](decision: DecisionRequest)(implicit F: Sync[F]): DecisionAction[F] =
    (community, vote) => (decision, vote) match {
      case (d, v) if d.decisionType != v.decisionType =>
        F.raiseError(UnequalDecisionTypes(d.decisionType, v.decisionType))

      case (_, Vote.DecisionSystemChangeVote(decisionType, allocation, resolution)) =>
        changeDecisionSystem(decisionType, allocation, resolution)(community)

      case (DecisionRequest.AddMemberRequest(_, _, member), Vote.AddMemberVote(toAdd)) =>
        if(toAdd) addCommunityMember(member)(community)
        else F.unit
    }

  private def changeDecisionSystem[F[_]](decisionType: DecisionType, allocation: InfluenceDistributionType, resolution: DecisionResolutionType)(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    for {
      _ <- community.store.allocation.update(_ + (decisionType -> allocation))
      _ <- community.store.resolution.update(_ + (decisionType -> resolution))
      _ <- community.events.publish1(CommunityEvents.DecisionSystemChanged(community.address, decisionType, allocation, resolution))
    } yield ()

  private def addCommunityMember[F[_]](member: Member)(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    community.store.members.create(member.address).void.*>(community.events.publish1(CommunityEvents.MemberAdded(member.address, member.nickname)))
}
