package kratia

import cats.effect.{ExitCode, IO, IOApp}

object kratia_main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      app <- kratia_app.KratiaInMem[IO]
    } yield ExitCode.Success

}
