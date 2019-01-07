package lambdaone.github

import cats.data.Kleisli
import cats.effect.IO
import com.typesafe.config.Config

case class GithubConfiguration(
  privateKey: String,
  appId: String,
  webhookSecret: String
)

object GithubConfiguration {

  def load: Kleisli[IO, Config, GithubConfiguration] =
    for {
      appId <- loadString("app-id")
      secret <- loadString("webhook-secret")
      privateKey <- loadFromSecret("github.pem")
    } yield GithubConfiguration(appId, secret, privateKey)

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
