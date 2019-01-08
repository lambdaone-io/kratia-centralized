package lambdaone.github

import cats.data.Kleisli
import cats.effect.{ConcurrentEffect, ContextShift, IO, Resource}
import com.typesafe.config.Config
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.Implicits.global

case class GithubConfiguration(
  privateKey: String,
  appId: String,
  webhookSecret: String,
  client: Resource[IO, Client[IO]]
)

object GithubConfiguration {

  def load(implicit cs: ContextShift[IO]): Kleisli[IO, Config, GithubConfiguration] =
    for {
      appId <- loadString("app-id")
      secret <- loadString("webhook-secret")
      privateKey <- loadFromSecret("github.pem")
      client = BlazeClientBuilder[IO](global).resource
    } yield GithubConfiguration(privateKey, appId, secret, client)

  private def loadString(path: String): Kleisli[IO, Config, String] =
    for {
      config <- Kleisli.ask[IO, Config]
      value <- Kleisli.liftF {
        IO { config.getString(s"github.$path") }
      }
    } yield value

  private def loadFromSecret(path: String): Kleisli[IO, Config, String] =
    for {
      config <- Kleisli.ask[IO, Config]
      value <- Kleisli.liftF {
        IO { scala.io.Source.fromResource(s"secret/$path").mkString }
      }
    } yield value

}
