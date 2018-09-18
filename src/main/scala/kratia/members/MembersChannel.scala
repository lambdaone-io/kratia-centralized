package kratia.members

import cats.effect.{Effect, Sync}
import cats.implicits._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import kratia.members.MembersEvents._
import kratia.utils.{EventChannel, KratiaChannels}

import scala.concurrent.ExecutionContext

case class MembersChannel[F[_]](channel: EventChannel[F, MembersEvents]) extends AnyVal {

  def publishNewMember(member: Member)(implicit F: Sync[F]): F[Unit] =
    channel.publish(MembersEvents.NewMember(member.copy(secret = None)))
}

object MembersChannel {

  def apply[F[_]](channels: KratiaChannels[F])(implicit F: Effect[F], ec: ExecutionContext): F[MembersChannel[F]] =
    for {
      channel <- EventChannel[F, MembersEvents]("members", MembersEvents.MembersBoot) {
        case MembersBoot => ("members_boot", Json.Null)
        case event: NewMember => ("new_member", event.asJson)
      }
      _ <- channels.registerChannel(channel)
    } yield MembersChannel[F](channel)
}
