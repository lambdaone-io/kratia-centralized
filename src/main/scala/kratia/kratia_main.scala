package kratia

import cats.effect.{IO, Timer}
import fs2.Stream
import fs2.StreamApp
import fs2.StreamApp.ExitCode
import kratia_app._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.dsl

import scala.concurrent.ExecutionContext

object kratia_main extends StreamApp[IO] {

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  implicit val timer: Timer[IO] = IO.timer

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    for {
      app <- Stream.eval(KratiaInMem[IO])
      server <- BlazeBuilder[IO]
        .bindHttp(8080, "localhost")
        .mountService(KratiaStaticFiles[IO](app, dsl.io), "/")
        .mountService(KratiaAPI[IO](app, dsl.io), "/api")
        .serve
    } yield server
}
