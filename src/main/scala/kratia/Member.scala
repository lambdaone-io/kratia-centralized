package kratia

import java.util.UUID

import cats.implicits._
import cats.effect.Sync
import kratia.state.Store
import kratia.utils.Utils
import kratia.utils.Utils.Address
import org.http4s.Status

object Member {


  /** Models */

  case class MembersGlobal[F[_]](members: Store[F, Member])

  case class Member(address: Address, nickname: String, reputation: Int, secret: Secret)

  case class Secret(value: UUID) extends AnyVal


  /** Failures */

  case object MemberNotFound extends RuntimeException with KratiaFailure {
    override def code: Status = Status.NotFound
    override def message: String = "Member not found."
  }


  /** Functions */

  def create[F[_]](nickname: String, members: MembersGlobal[F]): F[Member] =
    for {
      address <- Utils.genAddress
      secret <- genSecret
      member = Member(address, nickname, 0, secret)
      _ <- members.members.create(member)
    } yield member

  def authenticate[F[_]](secret: Secret)(members: MembersGlobal[F])(implicit F: Sync[F]): F[Member] =
    members.members.filter(_.secret == secret) >>= (_.headOption match {
      case Some(member) => member.pure[F]
      case None => F.raiseError(MemberNotFound)
    })

  private def genSecret[F[_]](implicit F: Sync[F]): F[Secret] =
    F.delay(Secret(UUID.randomUUID()))
}
