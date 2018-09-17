package kratia.communities

import cats.implicits._
import cats.effect.{ConcurrentEffect, Sync}
import fs2.async.mutable.Topic
import kratia.communities.communities_decision.{CommunityDependencies, DecisionResolutionType, DecisionType, InfluenceDistributionType}
import kratia.communities.communities_events.CommunityEvents
import kratia.communities.communities_events.CommunityEvents.CommunityCreated
import communities_collector.Collector
import kratia.kratia_core_model.{Address, Community}
import kratia.state.{State, Store}

import scala.concurrent.ExecutionContext

object communities_store {

  case class Communities[F[_]](communities: Store[F, Community[F]])

  case class CommunityStore[F[_]](
                                   members: Store[F, Address],
                                   active: Store[F, Collector[F]],
                                   allocation: State[F, Map[DecisionType, InfluenceDistributionType]],
                                   resolution: State[F, Map[DecisionType, DecisionResolutionType]]
                                 )

  def CommunitiesInMem[F[_]](implicit F: Sync[F]): F[Communities[F]] =
    for {
      communitiesStore <- Store.inMem[F, Community[F]]
    } yield Communities(communitiesStore)

  def CommunityInMem[F[_]](name: String)(global: Communities[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Community[F]] =
    for {
      address <- Address.gen[F]
      membersStore <- Store.inMem[F, Address]
      activeStore <- Store.inMem[F, Collector[F]]
      allocationState <- State.inMem[F, Map[DecisionType, InfluenceDistributionType]](Map.empty)
      resolutionState <- State.inMem[F, Map[DecisionType, DecisionResolutionType]](Map.empty)
      store = CommunityStore[F](membersStore, activeStore, allocationState, resolutionState)
      dependencies = CommunityDependencies(communities_collector.CollectorInMem[F])
      topic <- Topic[F, CommunityEvents](CommunityCreated(name))
      community <- global.communities.create(Community[F](address, name, "root", topic, store, dependencies))
    } yield community.model

}
