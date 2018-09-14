package kratia

import cats.implicits._
import cats.effect.Sync
import fs2.Sink
import fs2.async.mutable.Topic
import kratia.kratia_core_model.Member

object members_events {

  /* Model */

  case class MembersTopic[F[_]] private (topic: Topic[F, MembersEvents]) extends AnyVal {

    def newMember(implicit F: Sync[F]): Sink[F, Member] =
      _.map(cleanNewMember).through(topic.publish)
  }

  object MembersTopic {

    def apply[F[_]](implicit F: Sync[F]): F[MembersTopic[F]] =
      Topic[F, MembersEvents](MembersEvents.MembersBoot).map(MembersTopic.apply)
  }

  sealed trait MembersEvents

  object MembersEvents {

    case object MembersBoot extends MembersEvents

    case class NewMember(member: Member) extends MembersEvents
  }


  /* Functions */

  private def cleanNewMember(member: Member): MembersEvents =
    MembersEvents.NewMember(member.copy(secret = None))
}
