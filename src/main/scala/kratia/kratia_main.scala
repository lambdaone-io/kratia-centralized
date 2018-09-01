package kratia

import cats.effect.{ExitCode, IO, IOApp}
import kratia_app._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.dsl

object kratia_main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      app <- KratiaInMem[IO]
      static = KratiaStaticFiles[IO](app, dsl.io)
      api = KratiaAPI[IO](app, dsl.io)
      _ <- BlazeBuilder[IO].bindHttp(8080, "localhost")
        .mountService(static, "/")
        .mountService(api, "/api")
        .start
    } yield ExitCode.Success
}
