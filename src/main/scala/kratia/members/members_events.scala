package kratia.members

import cats.effect.{ConcurrentEffect, Effect, Sync}
import cats.implicits._
import fs2.async.mutable.{Queue, Topic}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import kratia.kratia_core_model.{Interrupt, Member, runWithInterrupt}
import kratia.kratia_protocol.OutMessage
import kratia.kratia_protocol.ProtocolMessage.KratiaEvent
import kratia.members.members_events.MembersEvents.{MembersBoot, NewMember}

import scala.concurrent.ExecutionContext

object members_events {

  /* Model */

  case class MembersTopic[F[_]] private (topic: Topic[F, MembersEvents]) extends AnyVal {

    def publishNewMember(member: Member)(implicit F: Sync[F]): F[Unit] =
      topic.publish1(cleanNewMember(member))

    def subscribeInto(queue: Queue[F, OutMessage])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Interrupt[F]] =
      runWithInterrupt(topic.subscribe(5).map(toEvent).to(queue.enqueue))
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
