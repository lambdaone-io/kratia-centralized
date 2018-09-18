package kratia.communities

import cats.implicits._
import cats.effect.Effect
import io.circe.generic.auto._
import io.circe.syntax._
import kratia.communities.CommunityEvents._
import kratia.utils.{EventChannel, KratiaChannels}

import scala.concurrent.ExecutionContext

case class CommunityChannel[F[_]](channel: EventChannel[F, CommunityEvents]) extends AnyVal {

}

object CommunityChannel {

  def apply[F[_]](name: String, channels: KratiaChannels[F])(implicit F: Effect[F], ec: ExecutionContext): F[CommunityChannel[F]] =
    for {
      channel <- EventChannel[F, CommunityEvents](s"community:$name", CommunityEvents.CommunityBooted(name)) {
        case event: CommunityBooted => ("community_booted", event.asJson)
        case event: Voted => ("voted", event.asJson)
        case event: Resolved => ("resolved", event.asJson)
        case event: MemberAdded => ("member_added", event.asJson)
        case event: DecisionSystemChanged => ("decision_system_changed", event.asJson)
      }
      _ <- channels.registerChannel(channel)
    } yield CommunityChannel[F](channel)
}
