package lambdaone.kratia.registry

import java.util.UUID

import io.circe.{Decoder, Encoder}

case class Community(address: UUID) extends AnyVal

object Community {

  implicit def circeEncoder: Encoder[Community] =
    Encoder.encodeUUID.contramap(_.address)

  implicit def circeDecoder: Decoder[Community] =
    Decoder.decodeUUID.map(Community.apply)

}
