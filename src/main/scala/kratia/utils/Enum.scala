package kratia.utils

import io.circe.Json.JString
import io.circe.{Decoder, Encoder, Json}
import io.circe.DecodingFailure

trait Enum[A] {

  def show(a: A): String

  def lift(s: String): Option[A]

  def default: A

  def all: Set[A]

  def liftWithDefault(s: String): A = lift(s).getOrElse(default)

  def notOneOf(s: String): String = s"'$s' was not part of enum: (${all.mkString(" | ")})"
}

object Enum {

  object implicits {

    implicit def EnumEncoder[A](implicit enum: Enum[A]): Encoder[A] =
      a => JString(enum.show(a))

    implicit def EnumDecoder[A](implicit enum: Enum[A]): Decoder[A] =
      hcursor => for {
        string <- hcursor.as[String]
        en <- enum.lift(string) match {
          case Some(en) => Right(en)
          case None => DecodingFailure(enum.notOneOf(string), hcursor.history)
        }
      } yield en
  }
}
