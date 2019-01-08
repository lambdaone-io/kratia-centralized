package lambdaone.github

import cats.data.Kleisli
import cats.effect.{IO, Timer}
import lambdaone.github.models.{Installation, InstallationAccessToken}
import org.http4s.{EntityDecoder, Header, Headers, Method, Request, Uri}
import org.http4s.circe.{jsonDecoder, jsonEncoder, jsonEncoderOf, jsonOf}
import io.circe.generic.auto._
import pdi.jwt.{Jwt, JwtAlgorithm}

import scala.concurrent.duration._

object GitHubApi {

  type GitHub[A] = Kleisli[IO, GithubConfiguration, A]

  def pure[A](a: A): GitHub[A] = Kleisli.pure(a)

  def io[A](fa: IO[A]): GitHub[A] = Kleisli.liftF(fa)

  def ghConfig: GitHub[GithubConfiguration] = Kleisli.ask

  def realTime(implicit timer: Timer[IO]): GitHub[Long] =
    io(timer.clock.realTime(SECONDS))

  /**
    * curl -i -X POST \
    * -H "Authorization: Bearer YOUR_JWT" \
    * -H "Accept: application/vnd.github.machine-man-preview+json" \
    * https://api.github.com/app/installations/:installation_id/access_tokens
    *
    */
  def getAccessToken(installation: Installation)(implicit timer: Timer[IO]): GitHub[InstallationAccessToken] =
    for {
      client <- ghConfig.map(_.client)
      jwt <- createJWTGitHubApp
      request = Request[IO](
        method = Method.POST,
        uri = Uri.fromString(installation.access_tokens_url).right.get,
        headers = Headers(
          Header("Authorization", s"Bearer $jwt"),
          Header("Accept", "application/vnd.github.machine-man-preview+json")
        )
      )
      response <- io(client.use(_.expect[InstallationAccessToken](request)))
    } yield response

  private def createJWTGitHubApp(implicit timer: Timer[IO]): GitHub[String] =
    for {
      now <- realTime
      config <- ghConfig
      in10secs = now + 10
      appId = config.appId
      jwt = Jwt.encode(
        s"""{ "iat": $now, "exp": $in10secs, "iss": "$appId" }""",
        config.privateKey,
        JwtAlgorithm.RS256
      )
    } yield jwt

  implicit def installationAccessTokenDecoder: EntityDecoder[IO, InstallationAccessToken] =
    jsonOf[IO, InstallationAccessToken]
}

