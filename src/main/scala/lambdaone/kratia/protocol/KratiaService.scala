package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{ContextShift, IO, Timer}
import lambdaone.kratia.collector._
import lambdaone.kratia.registry.{Community, Member, Registry}
import lambdaone.toolbox.{TemporalPriorityQueue, UniqueGen}
import lambdaone.toolbox.mem.CRUDPickInMem
import org.http4s.implicits._
import cats.implicits._
import io.circe.Json
import io.circe.syntax._
import lambdaone.kratia.protocol.RegistryProtocol.{RegisterRequest, RegisterResponse}
import org.http4s.circe.{jsonDecoder, jsonEncoder, jsonEncoderOf, jsonOf}
import org.http4s._
import org.http4s.dsl.io._
import io.circe.generic.auto._
import lambdaone.kratia.protocol.CollectorProtocol.{CreateBallotBoxRequest, CreateBallotBoxResponse, SetVoteRequest, SetVoteResponse}
import org.http4s.headers
import org.http4s.server.Router
import org.http4s.server.middleware._
import cats.effect.implicits._
import lambdaone.github.{GitHubApi, GithubConfiguration}
import lambdaone.github.events.{InstallationEvent, PullRequestEvent}
import lambdaone.kratia.collector.Proposal.BinaryProposal
import lambdaone.kratia.resolution.Resolved
import lambdaone.kratia.utils.DecodeOp.ifDecodes
import lambdaone.toolbox.QueueTaskProcessor.WorkerShutDown

import scala.concurrent.duration._

/**
  * Current unique community 19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db
  */
case class KratiaService(
  uniqueGen: UniqueGen[IO, UUID],
  registry: Registry[IO, MemberData],
  collector: Collector[IO],
  resolved: Resolved[IO],
  decisionQueue: TemporalPriorityQueue[UUID],
  githubConfiguration: GithubConfiguration
)(implicit timer: Timer[IO], cs: ContextShift[IO]) {

  // LOGIC

  object v1collectorBL {

    def createBallot(req: CreateBallotBoxRequest): IO[CreateBallotBoxResponse] =
      for {
        now <- timer.clock.realTime(SECONDS)
        box <- collector.create(req.validBallot, now + req.closesOn, req.data)
        duration <- decisionQueue.enqueue(box.address, req.closesOn.seconds)
        _ = println("Will resolve decision in " + duration)
        res = CreateBallotBoxResponse(BallotMetadata(
          box, req.validBallot, req.closesOn, req.data
        ))
      } yield res

  }

  // HTTP

  val rootCommunity: Community = Community(UUID.fromString("19ce7b9b-a4da-4f9c-9838-c04fcb0ce9db"))

  def runResolver: IO[WorkerShutDown] = DecisionResolver(decisionQueue, collector, resolved).run

  val corsConfig: CORSConfig =
    CORSConfig(
      anyOrigin = false,
      allowedOrigins = Set("http://localhost:8000", "http://demo.lambdaone.io", "http://kratia.127.0.0.1.xip.io:9090"),
      allowCredentials = true,
      maxAge = 1.day.toSeconds
    )

  def app: HttpApp[IO] = {
    Router(
      "/" -> github.routes,
      "/api/v1" -> CORS(v1registry <+> v1collector, corsConfig),
    ).orNotFound
  }

  def auth(request: Request[IO])(program: (Member, MemberData) => IO[Response[IO]]): IO[Response[IO]] = {
    val Bearer = """^Bearer\s(.*)$""".r
    request.headers.get(headers.Authorization).fold(Forbidden()) { header =>
      header.value match {
        case Bearer(token) =>
          Json.fromString(token).as[UUID].fold[IO[Response[IO]]](_ => Forbidden(), { address =>
            val member = Member(address)
            registry.load(rootCommunity, member).flatMap {
              case None => Forbidden()
              case Some(data) => program(member, data)
            }
          })
        case _ =>
          Forbidden()
      }
    }
  }

  def v1registry: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case request@POST -> Root / "registry" =>

      implicit val decoder: EntityDecoder[IO, RegisterRequest] =
        jsonOf[IO, RegisterRequest]
      implicit val encoder: EntityEncoder[IO, RegisterResponse] =
        jsonEncoderOf[IO, RegisterResponse]

      for {
        req <- request.as[RegisterRequest]
        address <- uniqueGen.gen
        member = Member(address)
        _ <- registry.register(req.community, member, req.data)
        ok <- Ok(RegisterResponse(member))
      } yield ok

    case request@GET -> Root / "registry" =>
      auth(request) { case (_, data) =>
        Ok(data.nickname.value)
      }
  }

  def v1collector: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case request@POST -> Root / "collector" =>

      implicit val decoder: EntityDecoder[IO, CreateBallotBoxRequest] =
        jsonOf[IO, CreateBallotBoxRequest]
      implicit val encoder: EntityEncoder[IO, CreateBallotBoxResponse] =
        jsonEncoderOf[IO, CreateBallotBoxResponse]

      auth(request) { (_, _) =>
        for {
          req <- request.as[CreateBallotBoxRequest]
          res <- v1collectorBL.createBallot(req)
          ok <- Ok(res)
        } yield ok
      }

    case request@GET -> Root / "collector" =>

      auth(request) { case (_, _) =>
        for {
          list <- collector.listOpen
          ok <- Ok(Json.obj("data" -> list.asJson))
        } yield ok
      }

    case request@POST -> Root / "collector" / "vote" =>

      implicit val decoder: EntityDecoder[IO, SetVoteRequest] =
        jsonOf[IO, SetVoteRequest]
      implicit val encoder: EntityEncoder[IO, SetVoteResponse] =
        jsonEncoderOf[IO, SetVoteResponse]

      auth(request) { case (member, _) =>
        for {
          req <- request.as[SetVoteRequest]
          vote = Vote(member, req.vote)
          proof <- collector.vote(req.ballotBox, vote)
          ok <- Ok(SetVoteResponse(proof.proof))
        } yield ok
      }

    case request@GET -> Root / "collector" / "finished" =>

      auth(request) { case (_, _) =>
        for {
          list <- resolved.listClosed
          ok <- Ok(Json.obj("data" -> list.asJson))
        } yield ok
      }

  }

  object github {

    def installation(event: InstallationEvent): IO[Unit] =
      for {
        installation <- GitHubApi.getAccessToken(event.installation).run(githubConfiguration)
        _ <- IO { println(installation) }
      } yield ()

    def pullRequest(event: PullRequestEvent): IO[Unit] =
      event.action match {
        case "opened" =>
          pullRequestOpen(event)
        case other =>
          IO { println(Console.MAGENTA + "PR event: " + other + Console.RESET) }
      }

    def pullRequestOpen(event: PullRequestEvent): IO[Unit] =
      for {
        res <- v1collectorBL.createBallot(CreateBallotBoxRequest(
          validBallot = BinaryProposal.ballot,
          data = DecisionData(event.asJson.noSpaces),
          closesOn = 60
        ))
        _ <- IO { println(Console.MAGENTA + res + Console.RESET) }
      } yield ()

    def routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

      case request@POST -> Root / "event_handler" =>

        implicit val decoder: EntityDecoder[IO, InstallationEvent] =
          jsonOf[IO, InstallationEvent]

        for {
          _ <- IO { println(request.headers) }
          json <- request.as[Json]
          _ <- {
            ifDecodes[InstallationEvent](installation) orElse
              ifDecodes[PullRequestEvent](pullRequest)
          }.run(json)
          ok <- Ok()
        } yield ok
    }
  }

}

