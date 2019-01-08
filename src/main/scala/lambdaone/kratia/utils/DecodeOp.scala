package lambdaone.kratia.utils

import cats.effect.IO
import io.circe.{Decoder, DecodingFailure, Json}
import lambdaone.kratia.utils.DecodeOp.DecodeOpFailure

case class DecodeOp[A, R](f: A => IO[R])(implicit decoder: Decoder[A]) {

  def run(json: Json): IO[R] =
    decoder.decodeJson(json) match {
      case Left(e) => IO.raiseError(DecodeOpFailure(e.message))
      case Right(a) => f(a)
    }

  def orElse[B](other: DecodeOp[B, R])(implicit od: Decoder[B]): DecodeOp[Either[A, B], R] =
    DecodeOp[Either[A, B], R]({
      case Left(a) => f(a)
      case Right(b) => other.f(b)
    })(DecodeOp.decodeFirst)

}

object DecodeOp {

  def ifDecodes[A]: DecodeOpPartial[A] = DecodeOpPartial()

  case class DecodeOpPartial[A]() {

    def apply[R](f: A => IO[R])(implicit da: Decoder[A]): DecodeOp[A, R] = DecodeOp(f)
  }

  case class DecodeOpFailure(message: String) extends RuntimeException {

    override def getMessage: String = message
  }

  def decodeFirst[A, B](implicit da: Decoder[A], db: Decoder[B]): Decoder[Either[A, B]] =
    Decoder.instance[Either[A, B]] { hcursor =>
      da(hcursor) match {
        case Right(a) => Right(Left(a))
        case Left(ae) =>
          db(hcursor) match {
            case Right(b) => Right(Right(b))
            case Left(be) =>
              Left(DecodingFailure(s"Couldn't decode A or B where A || ${ae.history} || and B || ${be.history} ||", hcursor.history))
          }
      }
    }
}
