package lambdaone.kratia.protocol

import cats.Functor
import io.circe.{Decoder, Encoder}
import lambdaone.kratia.protocol.MemberData.Nickname
import lambdaone.kratia.registry.Registry

case class MemberData(nickname: Nickname)

object MemberData {

  case class Nickname(value: String) extends AnyVal

  object Nickname {

    implicit def circeEncoder: Encoder[Nickname] =
      Encoder.encodeString.contramap(_.value)

    implicit def circeDecoder: Decoder[Nickname] =
      Decoder.decodeString.map(Nickname.apply)

    implicit def nicknameRegistry[F[_]: Functor, A](implicit registry: Registry[F, A, MemberData]): Registry[F, A, Nickname] =
      registry.imap(_.nickname)(MemberData.apply)
  }

}
