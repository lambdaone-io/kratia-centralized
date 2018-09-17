package kratia

import io.circe.{Decoder, Encoder}
import java.util.UUID

import fs2.Stream
import fs2.async.mutable.Signal
import cats.Show
import cats.implicits._
import cats.effect.{ConcurrentEffect, Effect, Sync}
import kratia.members_auth.Secret
import kratia.members_events.MembersTopic
import kratia.members_store.MemberStore

import scala.concurrent.ExecutionContext

object kratia_core_model {

  case class Address(value: UUID) extends AnyVal

  object Address {

    def gen[F[_]](implicit sync: Sync[F]): F[Address] =
      sync.delay(Address(UUID.randomUUID()))

    implicit val jsonEncoder: Encoder[Address] =
      Encoder.encodeUUID.contramap(_.value)

    implicit val jsonDecoder : Decoder[Address] =
      Decoder.decodeUUID.map(Address.apply)
  }

  case class Member(address: Address, nickname: String, secret: Option[Secret])

  object Member {

    def create[F[_]](nickname: String)(store: MemberStore[F], events: MembersTopic[F])(implicit F: Effect[F], ec: ExecutionContext): F[Member] =
      for {
        address <- Address.gen[F]
        secret <- Secret.gen[F]
        member = Member(address, nickname, Some(secret))
        _ <- store.save(member)
        _ <- events.publishNewMember(member)
      } yield member

    implicit val showMember: Show[Member] =
      member => s"M(${member.nickname})]"
  }

  type Interrupt[F[_]] = F[Unit]

  def runWithInterrupt[F[_], A](stream: Stream[F, A])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): F[Interrupt[F]] =
    for {
      interrupt <- Signal[F, Boolean](false)
      _ <- F.start(stream.interruptWhen(interrupt).compile.drain)
    } yield interrupt.set(false)

}
