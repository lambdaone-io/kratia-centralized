package lambdaone.kratia.protocol

import java.util.UUID

import cats.effect.{ContextShift, IO, Timer}
import lambdaone.kratia.collector.{BallotBox, Collector}
import lambdaone.kratia.resolution.{DecisionResolution, Resolution, Resolved}
import lambdaone.kratia.resolution.DecisionResolution.Majority
import lambdaone.toolbox.TemporalPriorityQueue
import lambdaone.toolbox.QueueTaskProcessor
import lambdaone.toolbox.QueueTaskProcessor.WorkerShutDown

import scala.concurrent.duration._

case class DecisionResolver(queue: TemporalPriorityQueue[UUID], collector: Collector[IO], resolved: Resolved[IO])(implicit timer: Timer[IO], cs: ContextShift[IO]) {

  private val worker =
    new QueueTaskProcessor[UUID](
      parallelism = 1,
      restInterval = 1.second,
      maxRetries = 3,
      queue = queue
    )(resolve)

  def resolve(address: UUID): IO[Either[String, String]] =
    for {
      result <- collector.inspect(BallotBox(address))
      res = DecisionResolution(result.allocation, -1.0, Majority)
      resolution = Resolution(
        address = result.address,
        closedOn = result.closedOn,
        data = result.data,
        maxInfluence = result.maxInfluence,
        resolution = res
      )
      _ <- resolved.create(resolution)
    } yield Right(result.data + " DONE")

  def run: IO[WorkerShutDown] = worker.run

}
