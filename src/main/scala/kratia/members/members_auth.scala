package kratia.members

import java.util.UUID

import cats.effect.Sync
import cats.implicits._
import fs2.Pipe
import io.circe.{Decoder, Encoder}
import kratia.kratia_core_model.Member
import kratia.members.members_store.{MemberNotFound, MemberStore}

object members_auth {


  /** Models */

  case class Secret(value: UUID) extends AnyVal

  object Secret {

    def gen[F[_]](implicit F: Sync[F]): F[Secret] =
      F.delay(Secret(UUID.randomUUID()))

    implicit val jsonEncoder: Encoder[Secret] =
      Encoder.encodeUUID.contramap(_.value)

    implicit val jsonDecoder: Decoder[Secret] =
      Decoder.decodeUUID.map(Secret.apply)
  }


  /** Functions */

  def authenticate[F[_]](store: MemberStore[F])(implicit F: Sync[F]): Pipe[F, Secret, Member] =
    _ .evalMap(secret => store.store.filter(_.secret.contains(secret)))
      .evalMap[Member](_.headOption match {
        case Some(member) => member.pure[F]
        case None => F.raiseError(MemberNotFound)
      })
}
