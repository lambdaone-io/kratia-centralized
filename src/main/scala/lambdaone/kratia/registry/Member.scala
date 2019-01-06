package lambdaone.kratia.registry

import java.util.UUID

import io.circe.{Decoder, Encoder}

case class Member(address: UUID) extends AnyVal

object Member {

  implicit def circeEncoder: Encoder[Member] =
    Encoder.encodeUUID.contramap(_.address)

  implicit def circeDecoder: Decoder[Member] =
    Decoder.decodeUUID.map(Member.apply)

}
