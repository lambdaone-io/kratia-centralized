package kratia

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.generic.auto._
import kratia.members_auth.Secret
import org.http4s.Status

object kratia_protocol {

  sealed trait ProtocolMessage

  sealed trait InMessage extends ProtocolMessage

  sealed trait OutMessage extends ProtocolMessage

  sealed trait AuthenticatedMessage extends InMessage { val secret: Secret }

  object InMessage {

    case class Register(nickname: String) extends InMessage
    case object SubscribeMembers extends InMessage
    case class CreateCommunity(name: String, secret: Secret) extends AuthenticatedMessage

    implicit val decoder: Decoder[InMessage] =
      decode[Register]("register") or
      decode[InMessage]("create_community") or
      decodeEmpty("subscribe_members", SubscribeMembers)
  }

  object OutMessage {

    case class KratiaFailure(code: Status, message: String) extends Throwable with OutMessage

    case class LogFromServer(message: String) extends OutMessage

    implicit val encoder: Encoder[OutMessage] =
      Encoder {
        case message: KratiaFailure => encode[KratiaFailure]("failure").apply(message)
        case message: LogFromServer => encode[LogFromServer]("log_from_server").apply(message)
      }
  }


  /* Functions */

  private def decode[A <: InMessage](message: String)(implicit decoder: Decoder[A]): Decoder[InMessage] =
    Decoder { json =>
      for {
        name <- json.downField("message").as[String]
        _ <- {
          if (name == message) Right(())
          else Left(DecodingFailure("not a protocol message", json.history))
        }
        body <- json.downField("body").as[A]
      } yield body
    }

  private def decodeEmpty[A <: InMessage](message: String, a: A): Decoder[InMessage] =
    Decoder { json =>
      for {
        name <- json.downField("message").as[String]
        _ <- {
          if (name == message) Right(())
          else Left(DecodingFailure("not a protocol message", json.history))
        }
      } yield a
    }

  private def encode[A <: OutMessage](message: String)(implicit encoder: Encoder[A]): Encoder[OutMessage] =
    Encoder { a =>
      Json.obj(
        "message" -> Json.fromString(message),
        "body" -> encoder(a)
      )
    }
}
