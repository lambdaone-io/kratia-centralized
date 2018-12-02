package lambdaone.kratia

import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import lambdaone.kratia.protocol.{KratiaInDb, KratiaInMem}
import org.http4s.server.blaze._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    kratia <- KratiaInDb.inDb
    code <- BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(kratia.app)
      .serve.compile.drain.as(ExitCode.Success)
  } yield code

}
