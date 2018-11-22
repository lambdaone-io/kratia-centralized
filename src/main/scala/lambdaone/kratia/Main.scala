package lambdaone.kratia

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze._
import org.http4s.server.Router
import org.http4s.implicits._
import cats.implicits._
import io.circe.{Decoder, Encoder}
import lambdaone.kratia.protocol.{Kratia, MemberData, Register}
import lambdaone.kratia.protocol.MemberData.Nickname
import lambdaone.kratia.protocol.Register.{RegisterRequest, RegisterResponse}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.{EntityDecoder, EntityEncoder, HttpApp, HttpRoutes}
import org.http4s.dsl.io._
import io.circe.generic.auto._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    kratia <- Kratia.inMem
    code <- BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(app(kratia))
      .serve.compile.drain.as(ExitCode.Success)
  } yield code

  def app[A: Decoder: Encoder](kratia: Kratia[IO, A]): HttpApp[IO] =
    Router("/api/v1" -> v1kratia(implicitly[Decoder[A]], implicitly[Encoder[A]], kratia)).orNotFound

  def v1kratia[A: Decoder: Encoder](implicit kratia: Kratia[IO, A]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case request @ POST -> Root / "registry" =>
      implicit val decoder: EntityDecoder[IO, RegisterRequest[A, Nickname]] =
        jsonOf[IO, RegisterRequest[A, Nickname]]
      implicit val encoder: EntityEncoder[IO, RegisterResponse[A, Nickname]] =
        jsonEncoderOf[IO, RegisterResponse[A, Nickname]]
      for {
        req <- request.as[RegisterRequest[A, Nickname]]
        res <- Register.register[IO, A, Nickname](req)(
          Monad[IO],
          kratia.registry.imap(_.nickname)(MemberData.apply),
          kratia.uniqueGen
        )
        ok <- Ok(res)
      } yield ok
  }
}
