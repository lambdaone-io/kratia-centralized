package lambdaone.github

import cats.effect.IO
import io.circe.{Decoder, DecodingFailure, Json}
import lambdaone.github.GHEvent.GithubDecodingFailure

case class GHEvent[A](f: A => IO[Unit])(implicit decoder: Decoder[A]) {

  def run(json: Json): IO[Unit] =
    decoder.decodeJson(json) match {
      case Left(e) => IO.raiseError(GithubDecodingFailure(e.message))
      case Right(a) => f(a)
    }

  def orElse[B](other: GHEvent[B])(implicit od: Decoder[B]): GHEvent[Either[A, B]] =
    GHEvent[Either[A, B]]({
      case Left(a) => f(a)
      case Right(b) => other.f(b)
    })(GHEvent.decodeFirst)

}

object GHEvent {

  case class GithubDecodingFailure(message: String) extends RuntimeException {

    override def getMessage(): String = message
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
