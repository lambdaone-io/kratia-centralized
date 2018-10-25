package kratia.members

import cats.effect.{Concurrent, Effect, Sync}
import cats.implicits._
import fs2.Pipe
import kratia.Protocol.ProtocolMessage.KratiaFailure
import kratia.utils.{Address, KratiaChannels}
import lambdaone.toolbox.Store
import org.http4s.Status

import scala.concurrent.ExecutionContext

class MemberService[F[_]](store: Store[F, Member], events: MembersChannel[F]) {

  def create(nickname: String)(implicit F: Effect[F], ec: ExecutionContext): F[Member] =
    for {
      address <- Address.gen[F]
      secret <- Secret.gen[F]
      member = Member(address, nickname, Some(secret))
      _ <- store.create(member)
      _ <- events.publishNewMember(member)
    } yield member

  def authenticate(implicit F: Sync[F]): Pipe[F, Secret, Member] =
    _ .evalMap(secret => store.filter(_.secret.contains(secret)))
      .evalMap[F, Member](_.headOption match {
        case Some(member) => member.pure[F]
        case None => F.raiseError(MemberService.MemberNotFound)
      })
}

object MemberService {

  val MemberNotFound: KratiaFailure = KratiaFailure(Status.NotFound.code, "Member not found.")

  def inMem[F[_]](channels: KratiaChannels[F])(implicit F: Concurrent[F]): F[MemberService[F]] =
    for {
      store <- Store.inMem[F, Member]
      channel <- MembersChannel[F](channels)
    } yield new MemberService[F](store, channel)
}

