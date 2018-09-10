package kratia

import fs2.{Pipe, Scheduler, Sink, Stream}
import cats.implicits._
import cats.effect.{ConcurrentEffect, Sync, Timer}
import org.http4s.{HttpService, StaticFile, Status}
import kratia.kratia_member._
import kratia.kratia_community._
import kratia.kratia_configuration._
import kratia.utils.Log
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebsocketBits.{Text, WebSocketFrame}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object kratia_app {


  /** Models  */

  case class Kratia[F[_]](
    members: Members[F],
    communities: Communities[F],
    timeStream: Stream[F, Time],
    scheduler: Scheduler,
    apiLogger: Log[F]
  )

  trait KratiaFailure extends Throwable {

    def code: Status

    def message: String
  }

  case class Time(value: Long) extends AnyVal


  /** Functions */

  def KratiaInMem[F[_]](implicit timer: Timer[F], F: ConcurrentEffect[F], ec: ExecutionContext): Stream[F, Kratia[F]] =
    for {
      config <- Stream.eval(loadConfig[F])
      members <- Stream.eval(MembersInMem[F])
      communities <- Stream.eval(CommunitiesInMem[F])
      scheduler <- Scheduler[F](corePoolSize = 2)
      apiLogger = Log.colorPrint[F]("api")
    } yield Kratia[F](
      members,
      communities,
      timeStream[F](config),
      scheduler,
      apiLogger
    )

  def KratiaStaticFiles[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ GET -> Root =>
        StaticFile.fromResource("/static/index.html", Some(request)).getOrElseF(NotFound())

      case request @ GET -> "static" /: path =>
        StaticFile.fromResource("/static" + path.toString, Some(request)).getOrElseF(NotFound())
    }
  }

  def KratiaBroker[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case GET -> Root / "broker" =>
        val queue = fs2.async.unboundedQueue[F, WebSocketFrame]
        val stream: Pipe[F, WebSocketFrame, WebSocketFrame] = _.flatMap {
          case Text("subscribe_members", _) =>
            kratia.members.events.subscribe(5).map(event => Text(event.toString))
          case _ =>
            Stream.emit(Text("unknown_operation"))
        }

        queue.flatMap { q =>
          val d = q.dequeue.through(stream)
          val e = q.enqueue
          WebSocketBuilder[F].build(d, e)
        }
    }
  }

  def KratiaMembersAPI[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ POST -> Root =>
        request.as[String]
          .flatMap(MemberInMem(_)(kratia.members))
          .flatTap(member => kratia.apiLogger.info("Registered new member: " + member.nickname))
          .flatMap(member => Ok(member.asJson))
    }
  }

  private def timeStream[F[_]](config: KratiaConfig)(implicit timer: Timer[F], F: Sync[F]): Stream[F, Time] =
    Stream
      .duration[F]
      .evalMap(_ => timer.clockMonotonic(MILLISECONDS))
      .map(Time)
}
