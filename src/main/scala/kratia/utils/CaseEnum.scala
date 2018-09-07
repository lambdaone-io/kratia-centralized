package kratia.utils

import io.circe.{Decoder, Encoder}
import io.circe.DecodingFailure

trait CaseEnum[A] {

  def show(a: A): String

  def parse(s: String): Option[A]

  def default: A

  def members: Set[A]

  def parseOrDefault(s: String): A =
    parse(s).getOrElse(default)

  def errorMessage(s: String): String =
    s"'$s' was not part of enum: (${members.mkString(" | ")})"
}

object CaseEnum {

  object implicits {

    implicit def EnumEncoder[A](implicit enum: CaseEnum[A]): Encoder[A] =
      a => Encoder.encodeString(enum.show(a))

    implicit def EnumDecoder[A](implicit enum: CaseEnum[A]): Decoder[A] =
      hcursor => for {
        string <- hcursor.as[String]
        en <- enum.parse(string) match {
          case Some(en) => Right(en)
          case None => Left(DecodingFailure(enum.errorMessage(string), hcursor.history))
        }
      } yield en
  }
}
