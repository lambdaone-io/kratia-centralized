package kratia

import cats.implicits._
import cats.effect.{Effect, Sync}
import fs2.async.mutable.{Queue, Signal, Topic}
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import kratia.kratia_core_model.{DoUnsub, Member}
import kratia.kratia_protocol.OutMessage
import kratia.kratia_protocol.ProtocolMessage.KratiaEvent
import kratia.members_events.MembersEvents.{MembersBoot, NewMember}

import scala.concurrent.ExecutionContext

object members_events {

  /* Model */

  case class MembersTopic[F[_]] private (topic: Topic[F, MembersEvents]) extends AnyVal {

    def publishNewMember(member: Member)(implicit F: Sync[F]): F[Unit] =
      topic.publish1(cleanNewMember(member))

    def subscribeInto(queue: Queue[F, OutMessage])(implicit F: Effect[F], ec: ExecutionContext): F[DoUnsub[F]] =
      for {
        interrupt <- Signal[F, Boolean](false)
        _ <- topic.subscribe(5)
          .interruptWhen(interrupt)
          .map(toEvent)
          .to(queue.enqueue)
          .compile.drain
      } yield interrupt.set(true)
  }

  object MembersTopic {

    val NAME: String = "members"

    def apply[F[_]](implicit F: Effect[F], ec: ExecutionContext): F[MembersTopic[F]] =
      Topic[F, MembersEvents](MembersEvents.MembersBoot).map(topic => new MembersTopic(topic))
  }

  sealed trait MembersEvents

  object MembersEvents {

    case object MembersBoot extends MembersEvents

    case class NewMember(member: Member) extends MembersEvents
  }


  /* Functions */

  private def cleanNewMember(member: Member): MembersEvents =
    MembersEvents.NewMember(member.copy(secret = None))

  private def toEvent(event: MembersEvents): KratiaEvent =
    event match {
      case MembersBoot => createEvent("members_boot", Json.Null)
      case event: NewMember => createEvent("new_member", event.asJson)
    }

  private def createEvent(name: String, body: Json): KratiaEvent =
    KratiaEvent(MembersTopic.NAME, name, body)
}
