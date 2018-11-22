package lambdaone.kratia.protocol

import io.circe.{Decoder, Encoder}
import lambdaone.kratia.protocol.MemberData.Nickname

case class MemberData(nickname: Nickname)

object MemberData {

  case class Nickname(value: String) extends AnyVal

  object Nickname {

    implicit def circeEncoder: Encoder[Nickname] =
      Encoder.encodeString.contramap(_.value)

    implicit def circeDecoder: Decoder[Nickname] =
      Decoder.decodeString.map(Nickname.apply)

  }

}
