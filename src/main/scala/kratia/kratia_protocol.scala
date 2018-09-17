package kratia

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.generic.auto._
import kratia.kratia_core_model.Member
import kratia.members_auth.Secret

object kratia_protocol {

  sealed trait ProtocolMessage

  sealed trait InMessage extends ProtocolMessage

  sealed trait OutMessage extends ProtocolMessage

  sealed trait KrRequest extends InMessage

  sealed trait AuthMessage { self: InMessage => val secret: Secret }

  object ProtocolMessage {

    /* In */

    case class Subscribe(topic: String) extends InMessage

    case class Unsubscribe(topic: String) extends InMessage

    case class Register(nickname: String) extends KrRequest

    case class CreateCommunity(name: String, secret: Secret) extends InMessage with AuthMessage

    /* Out */

    case class Registered(member: Member) extends OutMessage

    case class KratiaFailure(code: Int, message: String) extends Throwable with OutMessage

    case class KratiaEvent(topic: String, name: String, body: Json) extends OutMessage

    case class LogFromServer(message: String) extends OutMessage

    implicit val decoder: Decoder[ProtocolMessage] =
      decode[Register]("register") or
      decode[Registered]("register") or
      decode[CreateCommunity]("create_community") or
      decode[Subscribe]("subscribe") or
      decode[Unsubscribe]("unsubscribe") or
      decode[KratiaFailure]("register") or
      decode[KratiaEvent]("event") or
      decode[LogFromServer]("create_community")

    implicit val encoder: Encoder[ProtocolMessage] =
      Encoder {
        case message: Register => encode[Register]("register", message)
        case message: Registered => encode[Registered]("registered", message)
        case message: CreateCommunity => encode[CreateCommunity]("create_community", message)
        case message: Subscribe => encode[Subscribe]("subscribe", message)
        case message: Unsubscribe => encode[Unsubscribe]("unsubscribe", message)
        case message: KratiaFailure => encode[KratiaFailure]("failure", message)
        case message: KratiaEvent => encode[KratiaEvent]("event", message)
        case message: LogFromServer => encode[LogFromServer]("log_from_server", message)
      }
  }


  /* Functions */

  private def decode[A <: ProtocolMessage](message: String)(implicit decoder: Decoder[A]): Decoder[ProtocolMessage] =
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

  private def decodeEmpty[A <: ProtocolMessage](message: String, a: A): Decoder[ProtocolMessage] =
    Decoder { json =>
      for {
        name <- json.downField("message").as[String]
        _ <- {
          if (name == message) Right(())
          else Left(DecodingFailure("not a protocol message", json.history))
        }
      } yield a
    }

  private def encode[A <: ProtocolMessage](message: String, a: A)(implicit encoder: Encoder[A]): Json =
    Json.obj(
      "message" -> Json.fromString(message),
      "body" -> encoder(a)
    )

  private def encodeEmpty(message: String): Json =
    Json.obj(
      "message" -> Json.fromString(message)
    )
}
