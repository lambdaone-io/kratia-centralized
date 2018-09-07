package kratia

import java.util.UUID

import cats.Monoid
import cats.implicits._
import cats.effect.{Concurrent, ConcurrentEffect, Sync}
import fs2.async.mutable.Topic
import kratia.state.{State, Store}
import kratia.kratia_collector.{vote => collectorVote, _}
import kratia.kratia_community.CommunityEvents.CommunityCreated
import kratia.kratia_app.KratiaFailure
import kratia.kratia_member.Member
import kratia.utils.Address
import kratia.utils.Address.genAddress
import org.http4s.Status

import scala.concurrent.ExecutionContext

object kratia_community {


  /** Models */

  case class Communities[F[_]](communities: Store[F, Community[F]])

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

  case class Decision(decisionType: DecisionType, description: String, domain: String)

  case class Influence(value: Long) extends AnyVal

  object Influence {

    implicit object InfluenceMonoid extends Monoid[Influence] {

      override def empty: Influence = Influence(0l)

      override def combine(x: Influence, y: Influence): Influence = Influence(x.value + y.value)
    }
  }

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

    /*
    case object LowInertia extends DecisionResolutionType("low_inertia")

    case object AtLeastOne extends DecisionResolutionType("at_least_one")

    case object Unanimous extends DecisionResolutionType("anonimous")
    */
  }

  sealed abstract class InfluenceDistributionType(val name: String)

  object InfluenceDistributionType {

    case object Democratic extends InfluenceDistributionType("democratic")

    case class Autocratic(control: Address) extends InfluenceDistributionType("autocratic")

    /*
    case object Oligarchic extends InfluenceDistributionType("oligarchic")

    case object Meritocratic extends InfluenceDistributionType("meritocratic")

    case object TemporalDegradation extends InfluenceDistributionType("temporal_degradation")

    case object GeolocationBased extends InfluenceDistributionType("geolocation_based")
    */
  }


  /** Events */

  sealed trait CommunityEvents

  object CommunityEvents {

    case class CommunityCreated(name: String) extends CommunityEvents

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

  def CommunitiesInMem[F[_]](implicit F: Sync[F]): F[Communities[F]] =
    for {
      communitiesState <- State.StateInMem[F, Map[UUID, Community[F]]](Map.empty)
      communitiesStore = Store.StoreFromState(communitiesState)
    } yield Communities(communitiesStore)

  def CommunityInMem[F[_]](name: String)(global: Communities[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Community[F]] =
    for {
      address <- genAddress[F]
      membersState <- State.StateInMem[F, Map[UUID, Address]](Map.empty)
      membersStore = Store.StoreFromState(membersState)
      activeState <- State.StateInMem[F, Map[UUID, Collector[F]]](Map.empty)
      activeStore = Store.StoreFromState(activeState)
      allocationState <- State.StateInMem[F, Map[DecisionType, InfluenceDistributionType]](Map.empty)
      resolutionState <- State.StateInMem[F, Map[DecisionType, DecisionResolutionType]](Map.empty)
      store = CommunityStore(membersStore, activeStore, allocationState, resolutionState)
      dependencies = CommunityDependencies(kratia_collector.CollectorInMem[F])
      topic <- Topic[F, CommunityEvents](CommunityCreated(name))
      community <- global.communities.create(Community[F](address, name, "root", topic, store, dependencies))
    } yield community.model

  def vote[F[_]](sender: Member, address: Address, vote: Vote)(community: Community[F])(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- authorize(sender)(community)
      collector <- community.store.active.get(address.value)(CollectorNotFound(address))
      proof <- collectorVote(sender, vote)(collector.model)
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
      result <- resolve(decision, ballot)(community)
      _ <- action(community, result)
      _ <- community.events.publish1(CommunityEvents.Resolved(address, decision, result))
    } yield ()

  private def authorize[F[_]](member: Member)(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    community.store.members.exists(member.address) >>= (exists => if (exists) F.unit else F.raiseError(UnauthorizedMember(member)))

  private def resolve[F[_]](decision: Decision, ballot: Ballot)(community: Community[F])(implicit F: Sync[F]): F[Vote] =
    for {
      allocationType <- community.store.allocation.get.map(_(decision.decisionType))
      resolutionType <- community.store.resolution.get.map(_(decision.decisionType))
      buckets <- influenceBuckets(ballot, allocationType)
      vote <- resolveVote(buckets, resolutionType)
    } yield vote

  private def influenceBuckets[F[_]](ballot: Ballot, allocationType: InfluenceDistributionType)(implicit F: Sync[F]): F[Map[Vote, Influence]] = {

    def boxInfluenceBuckets(buckets: List[(Vote, Influence)]): F[Map[Vote, Influence]] =
      buckets.groupBy(_._1).mapValues(_.foldLeft(Influence(0l))(_ |+| _._2)).pure[F]

    boxInfluenceBuckets(allocationType match {
      case InfluenceDistributionType.Autocratic(control) =>
        ballot.value.toList.map {
          case (member, vote) if member == control => (vote, Influence(1l))
          case (_, vote) => (vote, Influence(0l))
        }
      case InfluenceDistributionType.Democratic =>
        ballot.value.toList.map {
          case (_, vote) => (vote, Influence(1l))
        }
    })
  }

  private def resolveVote[F[_]](buckets: Map[Vote, Influence], resolutionType: DecisionResolutionType)(implicit F: Sync[F]): F[Vote] =
    resolutionType match {
      case DecisionResolutionType.Majority =>
        buckets.foldLeft((null.asInstanceOf[Vote], Influence(0l))) { (winning, x) =>
          if(x._2.value > winning._2.value) x else winning
        }._1.pure[F]
    }

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
