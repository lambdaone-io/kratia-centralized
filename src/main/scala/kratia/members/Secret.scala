package kratia.members

import java.util.UUID

import cats.effect.Sync
import io.circe.{Decoder, Encoder}

case class Secret(value: UUID) extends AnyVal

object Secret {

  def gen[F[_]](implicit F: Sync[F]): F[Secret] =
    F.delay(Secret(UUID.randomUUID()))

  implicit val jsonEncoder: Encoder[Secret] =
    Encoder.encodeUUID.contramap(_.value)

  implicit val jsonDecoder: Decoder[Secret] =
    Decoder.decodeUUID.map(Secret.apply)
}

