package kratia.communities

import cats.Monoid
import cats.effect.Sync
import cats.implicits._
import kratia.Protocol.ProtocolMessage.KratiaFailure
import kratia.communities.Community._
import kratia.communities.Collector._
import kratia.members.Member
import kratia.utils.Address
import lambdaone.toolbox.{State, Store}
import org.http4s.Status

case class Community[F[_]](
  address: Address,
  name: String,
  domain: String,
  events: CommunityChannel[F],
  store: CommunityStore[F],
  dependencies: CommunityDependencies[F]
) { self =>

  def vote(sender: Member, address: Address, vote: Vote)(implicit F: Sync[F]): F[ProofOfVote] =
    for {
      _ <- authorize(sender)
      collector <- store.active.get(address.value)(CollectorNotFound(address))
      proof <- collector.model.vote(sender, vote)
      _ <- events.channel.publish(CommunityEvents.Voted(vote, collector.model.decision))
    } yield proof

  def decide(sender: Member, request: DecisionRequest)(implicit F: Sync[F]): F[Address] =
    authorize(sender)
      .*>(dependencies.newCollector(request.toDecision, self, resolveAction(request)))
      .map(_.address)

  def reportDecision(collector: Collector[F])(implicit F: Sync[F]): F[Unit] =
    for {
      ballot <- collector.store.ballot.get
      (decision, address, action) = (collector.decision, collector.address, collector.action)
      result <- resolve(decision, ballot)
      _ <- action(result)
      _ <- events.channel.publish(CommunityEvents.Resolved(address, decision, result))
    } yield ()

  private def authorize(member: Member)(implicit F: Sync[F]): F[Unit] =
    store.members.exists(member.address) >>= (exists => if (exists) F.unit else F.raiseError(UnauthorizedMember(member)))

  private def resolve(decision: Decision, ballot: Ballot)(implicit F: Sync[F]): F[Vote] =
    for {
      allocationType <- store.allocation.get.map(_(decision.decisionType))
      resolutionType <- store.resolution.get.map(_(decision.decisionType))
      buckets <- influenceBuckets(ballot, allocationType)
      vote <- resolveVote(buckets, resolutionType)
    } yield vote

  private def influenceBuckets(ballot: Ballot, allocationType: InfluenceDistributionType)(implicit F: Sync[F]): F[Map[Vote, Influence]] = {

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

  private def resolveVote(buckets: Map[Vote, Influence], resolutionType: DecisionResolutionType)(implicit F: Sync[F]): F[Vote] =
    resolutionType match {
      case DecisionResolutionType.Majority =>
        buckets.foldLeft((null.asInstanceOf[Vote], Influence(0l))) { (winning, x) =>
          if(x._2.value > winning._2.value) x else winning
        }._1.pure[F]
    }

  private def resolveAction(decision: DecisionRequest)(implicit F: Sync[F]): DecisionAction[F] =
    vote => (decision, vote) match {
      case (d, v) if d.decisionType != v.decisionType =>
        F.raiseError(UnequalDecisionTypes(d.decisionType, v.decisionType))

      case (_, Vote.DecisionSystemChangeVote(decisionType, allocation, resolution)) =>
        changeDecisionSystem(decisionType, allocation, resolution)

      case (DecisionRequest.AddMemberRequest(_, _, member), Vote.AddMemberVote(toAdd)) =>
        if(toAdd) addCommunityMember(member)
        else F.unit
    }

  private def changeDecisionSystem(decisionType: DecisionType, allocation: InfluenceDistributionType, resolution: DecisionResolutionType)(implicit F: Sync[F]): F[Unit] =
    for {
      _ <- store.allocation.update(_ + (decisionType -> allocation))
      _ <- store.resolution.update(_ + (decisionType -> resolution))
      _ <- events.channel.publish(CommunityEvents.DecisionSystemChanged(address, decisionType, allocation, resolution))
    } yield ()

  private def addCommunityMember(member: Member)(implicit F: Sync[F]): F[Unit] =
    store.members.create(member.address).void.*>(events.channel.publish(CommunityEvents.MemberAdded(member.address, member.nickname)))
}


object Community {

  case class CommunityStore[F[_]](
    members: Store[F, Address],
    active: Store[F, Collector[F]],
    allocation: State[F, Map[DecisionType, InfluenceDistributionType]],
    resolution: State[F, Map[DecisionType, DecisionResolutionType]]
  )

  case class CommunityDependencies[F[_]](
    newCollector: (Decision, Community[F], DecisionAction[F]) => F[Collector[F]]
  )

  type DecisionAction[F[_]] = Vote => F[Unit]

  case class Decision(decisionType: DecisionType, description: String, domain: String)

  case class Influence(value: Long) extends AnyVal

  object Influence {

    implicit def influenceMonoid: Monoid[Influence] =
      new Monoid[Influence] {

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

  /** Failures */

  def UnauthorizedMember(member: Member): KratiaFailure =
    KratiaFailure(Status.Unauthorized.code, s"Operation cannot be performed for member ${member.nickname}")

  def UnequalDecisionTypes(a: DecisionType, b: DecisionType): KratiaFailure =
    KratiaFailure(Status.InternalServerError.code, s"$a =/= $b this state should never be reached")

}
