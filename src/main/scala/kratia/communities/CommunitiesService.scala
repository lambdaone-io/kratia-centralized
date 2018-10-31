package kratia.communities

import cats.implicits._
import cats.effect.{ConcurrentEffect, Effect, Concurrent}
import kratia.communities.Community._
import kratia.utils.{Address, KratiaChannels}
import lambdaone.toolbox.{State, Store}

import scala.concurrent.ExecutionContext

case class CommunitiesService[F[_]](store: Store[F, Community[F]], events: CommunitiesChannel[F]) {

  def create(name: String, channels: KratiaChannels[F])(implicit F: ConcurrentEffect[F]): F[Community[F]] =
    for {
      address <- Address.gen[F]
      membersStore <- Store.inMem[F, Address]
      activeStore <- Store.inMem[F, Collector[F]]
      allocationState <- State.inMem[F, Map[DecisionType, InfluenceDistributionType]](Map.empty)
      resolutionState <- State.inMem[F, Map[DecisionType, DecisionResolutionType]](Map.empty)
      channel <- CommunityChannel[F](name, channels)
      cstore= CommunityStore[F](membersStore, activeStore, allocationState, resolutionState)
      dependencies = CommunityDependencies(Collector.inMem[F])
      community <- store.create(Community[F](address, name, "root", channel, cstore, dependencies))
    } yield community.model
}

object CommunitiesService {

  def inMem[F[_]](channels: KratiaChannels[F])(implicit F: Concurrent[F]): F[CommunitiesService[F]] =
    for {
      communitiesStore <- Store.inMem[F, Community[F]]
      channel <- CommunitiesChannel[F](channels)
    } yield CommunitiesService(communitiesStore, channel)
}
