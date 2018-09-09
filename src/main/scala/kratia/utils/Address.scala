package kratia.utils

import io.circe.{Decoder, Encoder}
import java.util.UUID

import cats.effect.Sync

case class Address(value: UUID) extends AnyVal

object Address {

  def genAddress[F[_]](implicit sync: Sync[F]): F[Address] =
    sync.delay(Address(UUID.randomUUID()))

  implicit val jsonEncoder: Encoder[Address] =
    Encoder.encodeUUID.contramap(_.value)

  implicit val jsonDecoder : Decoder[Address] =
    Decoder.decodeUUID.map(Address.apply)
}
