package kratia.communities

import cats.implicits._
import cats.effect.{Effect, Sync}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Json
import kratia.communities.CommunitiesEvents._
import kratia.utils.{EventChannel, KratiaChannels}

import scala.concurrent.ExecutionContext

case class CommunitiesChannel[F[_]](channel: EventChannel[F, CommunitiesEvents]) extends AnyVal {

  def publishNewCommunity(community: Community[F])(implicit F: Sync[F]): F[Unit] =
    channel.publish(CommunityCreated(community.address, community.name, community.domain))
}

object CommunitiesChannel {

  def apply[F[_]](channels: KratiaChannels[F])(implicit F: Effect[F], ec: ExecutionContext): F[CommunitiesChannel[F]] =
    for {
      channel <- EventChannel[F, CommunitiesEvents]("communities", CommunitiesEvents.CommunitiesBooted) {
        case CommunitiesBooted => ("communities_booted", Json.Null)
        case event: CommunityCreated => ("community_created", event.asJson)
      }
      _ <- channels.registerChannel(channel)
    } yield CommunitiesChannel[F](channel)
}
