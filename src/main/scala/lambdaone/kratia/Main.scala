package lambdaone.kratia

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import lambdaone.github.GithubConfiguration
import lambdaone.kratia.protocol.KratiaInMem
import org.http4s.server.blaze._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      config <- buildConfigObject
      githubConfig <- GithubConfiguration.load.run(config)
      _ = println(githubConfig)
      kratia <- KratiaInMem.inMem
      workerShutdown <- kratia.runResolver
      code <- BlazeServerBuilder[IO]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(kratia.app)
        .serve.compile.drain.as(ExitCode.Success)
    } yield code

  def buildConfigObject: IO[Config] =
    IO { ConfigFactory.load() }
}
