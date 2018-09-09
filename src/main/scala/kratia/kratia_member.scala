package kratia

import java.util.UUID

import cats.implicits._
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import kratia.state.State
import kratia.kratia_app.KratiaFailure
import kratia.state.Store
import kratia.utils.Address
import kratia.utils.Address.genAddress
import org.http4s.Status

object kratia_member {


  /** Models */

  case class Members[F[_]](members: Store[F, Member])

  case class Member(address: Address, nickname: String, secret: Secret)

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


  /** Functions */

  def MembersInMem[F[_]](implicit F: Sync[F]): F[Members[F]] =
    for {
      membersState <- State.StateInMem[F, Map[UUID, Member]](Map.empty)
      membersStore = Store.StoreFromState(membersState)
    } yield Members(membersStore)

  def MemberInMem[F[_]](nickname: String)(store: Members[F])(implicit F: Sync[F]): F[Member] =
    for {
      address <- genAddress
      secret <- genSecret
      member: Member = Member(address, nickname, secret)
      _ <- store.members.create(member)
    } yield member

  def authenticate[F[_]](secret: Secret)(members: Members[F])(implicit F: Sync[F]): F[Member] =
    members.members.filter(_.secret == secret) >>= (_.headOption match {
      case Some(member) => member.pure[F]
      case None => F.raiseError(MemberNotFound)
    })

  private def genSecret[F[_]](implicit F: Sync[F]): F[Secret] =
    F.delay(Secret(UUID.randomUUID()))
}
