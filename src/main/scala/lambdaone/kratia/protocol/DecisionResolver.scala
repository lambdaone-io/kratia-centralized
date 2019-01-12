package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{ContextShift, IO, Timer}
import io.circe.Json
import lambdaone.github.GitHubApi
import lambdaone.kratia.collector.{BallotBox, Collector, DecisionData}
import lambdaone.kratia.resolution.{DecisionResolution, Resolution, Resolved}
import lambdaone.kratia.resolution.DecisionResolution.Majority
import lambdaone.toolbox.TemporalPriorityQueue
import lambdaone.toolbox.QueueTaskProcessor
import lambdaone.toolbox.QueueTaskProcessor.WorkerShutDown
import lambdaone.kratia.utils.DecodeOp.ifDecodes
import io.circe.generic.auto._
import io.circe.syntax._
import lambdaone.github.events.PullRequestEvent
import lambdaone.kratia.collector.Proposal.BinaryProposal

import scala.concurrent.duration._

case class DecisionResolver(
    queue: TemporalPriorityQueue[UUID],
    collector: Collector[IO],
    resolved: Resolved[IO],
    gitHubApi: GitHubApi[IO]
  )(implicit timer: Timer[IO], cs: ContextShift[IO]) {

  private val worker =
    new QueueTaskProcessor[UUID](
      parallelism = 1,
      restInterval = 2.second,
      maxRetries = 3,
      queue = queue
    )(resolve)

  def resolve(address: UUID): IO[Either[String, String]] =
    for {
      result <- collector.inspect(BallotBox(address))
      resolution0 = DecisionResolution(result.allocation, -1.0, Majority)
      newData <- {
        ifDecodes[PullRequestEvent] { event =>
          if (resolution0.headOption.contains(BinaryProposal.Yes))
            gitHubApi.closePullRequest(event.pull_request, event.installation.id).map(_ => event.asJson)
          else
            IO.pure(Json.obj())
        } orElse
        ifDecodes[SimpleDecision] { simple =>
          IO.pure(simple.message)
        }
      }.run(result.data.value)
      resolution = Resolution(
        address = result.address,
        closedOn = result.closedOn,
        data = DecisionData(newData),
        maxInfluence = result.maxInfluence,
        resolution = resolution0
      )
      _ <- resolved.create(resolution)
    } yield Right(result.data + " DONE")

  def run: IO[WorkerShutDown] = worker.run

}
