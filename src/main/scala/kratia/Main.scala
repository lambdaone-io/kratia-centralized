package kratia

import cats.effect.{IO, Timer}
import fs2.Stream
import fs2.StreamApp
import fs2.StreamApp.ExitCode
import App._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.dsl

import scala.concurrent.ExecutionContext

object Main extends StreamApp[IO] {

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  implicit val timer: Timer[IO] = IO.timer

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    for {
      app <- KratiaInMem[IO]
      server <- BlazeBuilder[IO]
        .bindHttp(8080, "localhost")
        .mountService(KratiaStaticFiles[IO](dsl.io), "/")
        .mountService(KratiaBroker[IO](dsl.io, app), "/api")
        .serve
    } yield server
}
