package lambdaone.kratia

import java.util.UUID

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze._
import org.http4s.server.Router
import org.http4s.implicits._
import cats.implicits._
import lambdaone.kratia.protocol.Kratia
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.dsl.io._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    kratia <- ???
    code <- BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(app(kratia))
      .serve.compile.drain.as(ExitCode.Success)
  } yield code

  def app(kratia: Kratia[IO, UUID]): HttpApp[IO] = Router("/v1/api" -> v1kratia(kratia)).orNotFound

  def v1kratia(implicit kratia: Kratia[IO, UUID]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request @ POST -> Root / "registry" => ???
  }
}
