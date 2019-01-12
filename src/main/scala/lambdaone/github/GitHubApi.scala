package lambdaone.github

import cats.{MonadError, Functor}
import cats.implicits._
import cats.effect.{IO, Sync, Timer}
import lambdaone.github.models._
import org.http4s.{EntityDecoder, Header, Headers, Method, Request, Uri}
import org.http4s.circe.{jsonEncoder, jsonOf, jsonDecoder}
import io.circe.generic.auto._
import lambdaone.github.GitHubApi.NoTokenStoredForInstallation
import lambdaone.toolbox.CRUDPick
import pdi.jwt.{Jwt, JwtAlgorithm}
import com.github.nscala_time.time.Imports._
import io.circe.Json
import io.circe.syntax._
import lambdaone.kratia.utils.DecodeOp

import scala.concurrent.duration._

trait GitHubApi[F[_]] {

  def closePullRequest(pr: PullRequest, installationId: Int): F[Either[MergePRFailure, MergePRSuccess]]

  def getAccessToken(installation: Installation): F[InstallationAccessToken]

  def storeAccessToken(installation: Installation, token: InstallationAccessToken): F[Unit]

  def fetchAccessTokenFromStore(id: Int): F[Option[(Installation, InstallationAccessToken)]]

  def removeAccessTokenFromStore(id: Int): F[Unit]

  def getOrRenewAccessToken(installationId: Int)(implicit F: MonadError[F, Throwable]): F[InstallationAccessToken] =
    for {
      optToken <- fetchAccessTokenFromStore(installationId)
      token <- optToken match {
        case None => F.raiseError(NoTokenStoredForInstallation(installationId))
        case Some((installation, token0)) =>
          val now = DateTime.now().minusMinutes(1).getMillis
          val expires = token0.expiresAt.getMillis
          println(Console.RED + "NOW: " + now + "  TOKEN EXPIRES IN: " + expires + Console.RESET)
          if (now < expires) F.pure(token0)
          else for {
            token1 <- getAccessToken(installation)
            _ <- storeAccessToken(installation, token1)
          } yield token1
      }
    } yield token

}

object GitHubApi {

  case class NoTokenStoredForInstallation(installation: Int) extends Exception("Can't find token for installation " + installation.toString)

}

case class GitHubApiIO(config: GitHubConfig, crudTokens: CRUDPick[IO, Int, (Installation, InstallationAccessToken)])(implicit timer: Timer[IO]) extends GitHubApi[IO] {

  def closePullRequest(pr: PullRequest, installationId: Int): IO[Either[MergePRFailure, MergePRSuccess]] =
    for {
      token <- getOrRenewAccessToken(installationId)
      res <- requestClosePullRequest(pr, token)
      _ = println(Console.GREEN + res + Console.RESET)
    } yield res

  def storeAccessToken(installation: Installation, token: InstallationAccessToken): IO[Unit] =
    crudTokens.create(installation -> token, installation.id).void

  def fetchAccessTokenFromStore(id: Int): IO[Option[(Installation, InstallationAccessToken)]] =
    crudTokens.get(id)

  /**
    * curl -i -X POST \
    * -H "Authorization: Bearer YOUR_JWT" \
    * -H "Accept: application/vnd.github.machine-man-preview+json" \
    * https://api.github.com/app/installations/:installation_id/access_tokens
    *
    */
  def getAccessToken(installation: Installation): IO[InstallationAccessToken] =
    for {
      jwt <- createJWTGitHubApp
      request = Request[IO](
        method = Method.POST,
        uri = Uri.fromString(installation.access_tokens_url).right.get,
        headers = Headers(
          Header("Authorization", s"Bearer $jwt"),
          Header("Accept", "application/vnd.github.machine-man-preview+json")
        )
      )
      response <- config.client.use(_.expectOr[InstallationAccessToken](request) { response0 =>
        response0.as[Json](Functor[IO], jsonDecoder).map { json =>
          println(Console.RED + json.spaces2 + Console.RESET)
          new Exception(json.noSpaces)
        }
      })
    } yield response

  def removeAccessTokenFromStore(id: Int): IO[Unit] =
    crudTokens.delete(id).void

  private def requestClosePullRequest(pr: PullRequest, token: InstallationAccessToken): IO[Either[MergePRFailure, MergePRSuccess]] = {
    println(Console.YELLOW + s"Will try to merge PR: ${pr._links.self.href}" + Console.RESET)
    val request = Request[IO](
      method = Method.PUT,
      uri = Uri.fromString(s"${pr._links.self.href}/merge").right.get,
      headers = Headers(
        Header("Authorization", s"token ${token.token}"),
        Header("Accept", "application/vnd.github.machine-man-preview+json")
      ),
      body = jsonEncoder[IO].toEntity(Json.obj(
        "commit_title" -> "Kratia merge with results: ".asJson,
        "commit_message" -> "Full results were...".asJson,
        "sha" -> pr.head.sha.asJson,
        "merge_method" -> "merge".asJson
      )).body
    )
    config.client.use(_.expectOr[Either[MergePRFailure, MergePRSuccess]](request)({ response0 =>
      response0.as[Json](Functor[IO], jsonDecoder).map { json =>
        println(Console.RED + json.spaces2 + Console.RESET)
        new Exception(json.noSpaces)
      }
    })(jsonOf(Sync[IO], DecodeOp.decodeFirst[MergePRFailure, MergePRSuccess])))
  }

  private def createJWTGitHubApp(implicit timer: Timer[IO]): IO[String] =
    for {
      now <- timer.clock.realTime(SECONDS)
      in10 = now + 10
      appId = config.appId
      jwt = Jwt.encode(
        s"""{ "iat": $now, "exp": $in10, "iss": "$appId" }""",
        config.privateKey,
        JwtAlgorithm.RS256
      )
    } yield jwt

  implicit def installationAccessTokenDecoder: EntityDecoder[IO, InstallationAccessToken] =
    jsonOf[IO, InstallationAccessToken]

}

