package kratia.utils

import io.circe.{Decoder, Encoder}
import io.circe.DecodingFailure

trait CaseEnum[A] {

  def show(a: A): String

  def lift(s: String): Option[A]

  def default: A

  def all: Set[A]

  def liftWithDefault(s: String): A = lift(s).getOrElse(default)

  def notOneOf(s: String): String = s"'$s' was not part of enum: (${all.mkString(" | ")})"
}

object CaseEnum {

  object implicits {

    implicit def EnumEncoder[A](implicit enum: CaseEnum[A]): Encoder[A] =
      a => Encoder.encodeString(enum.show(a))

    implicit def EnumDecoder[A](implicit enum: CaseEnum[A]): Decoder[A] =
      hcursor => for {
        string <- hcursor.as[String]
        en <- enum.lift(string) match {
          case Some(en) => Right(en)
          case None => Left(DecodingFailure(enum.notOneOf(string), hcursor.history))
        }
      } yield en
  }
}
