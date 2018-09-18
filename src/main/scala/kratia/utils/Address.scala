package kratia.utils

import java.util.UUID

import cats.effect.Sync
import io.circe.{Decoder, Encoder}

case class Address(value: UUID) extends AnyVal

object Address {

  def gen[F[_]](implicit sync: Sync[F]): F[Address] =
    sync.delay(Address(UUID.randomUUID()))

  implicit val jsonEncoder: Encoder[Address] =
    Encoder.encodeUUID.contramap(_.value)

  implicit val jsonDecoder : Decoder[Address] =
    Decoder.decodeUUID.map(Address.apply)
}


