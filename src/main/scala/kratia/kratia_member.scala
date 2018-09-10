package kratia

import java.util.UUID

import cats.implicits._
import cats.effect.{Effect, Sync}
import fs2.async.mutable.Topic
import io.circe.{Decoder, Encoder}
import kratia.state.State
import kratia.kratia_app.KratiaFailure
import kratia.state.Store
import kratia.utils.Address
import kratia.utils.Address.genAddress
import org.http4s.Status

import scala.concurrent.ExecutionContext

object kratia_member {


  /** Models */

  case class Members[F[_]](members: Store[F, Member], events: Topic[F, MembersEvents])

  case class Member(address: Address, nickname: String, secret: Option[Secret])

  case class Secret(value: UUID) extends AnyVal

  object Secret {

    implicit val jsonEncoder: Encoder[Secret] =
      Encoder.encodeUUID.contramap(_.value)

    implicit val jsonDecoder: Decoder[Secret] =
      Decoder.decodeUUID.map(Secret.apply)
  }


  /** Failures */

  case object MemberNotFound extends RuntimeException with KratiaFailure {
    override def code: Status = Status.NotFound
    override def message: String = "Member not found."
  }


  /** Events */

  sealed trait MembersEvents

  object MembersEvents {

    case object MembersBoot extends MembersEvents

    case class NewMember(member: Member) extends MembersEvents
  }

  /** Functions */

  def MembersInMem[F[_]](implicit F: Effect[F], ec: ExecutionContext): F[Members[F]] =
    for {
      membersState <- State.StateInMem[F, Map[UUID, Member]](Map.empty)
      events <- Topic[F, MembersEvents](MembersEvents.MembersBoot)
      membersStore = Store.StoreFromState(membersState)
    } yield Members(membersStore, events)

  def MemberInMem[F[_]](nickname: String)(members: Members[F])(implicit F: Sync[F]): F[Member] =
    for {
      address <- genAddress
      secret <- genSecret
      member: Member = Member(address, nickname, Some(secret))
      _ <- members.members.create(member)
      _ <- members.events.publish1(MembersEvents.NewMember(member.copy(secret = None)))
    } yield member

  def authenticate[F[_]](secret: Secret)(members: Members[F])(implicit F: Sync[F]): F[Member] =
    members.members.filter(_.secret.contains(secret)) >>= (_.headOption match {
      case Some(member) => member.pure[F]
      case None => F.raiseError(MemberNotFound)
    })

  private def genSecret[F[_]](implicit F: Sync[F]): F[Secret] =
    F.delay(Secret(UUID.randomUUID()))
}
