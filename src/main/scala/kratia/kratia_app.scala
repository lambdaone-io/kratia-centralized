package kratia

import cats.Show
import fs2.{Pipe, Scheduler, Sink, Stream}
import cats.implicits._
import cats.effect.{ConcurrentEffect, Sync, Timer}
import org.http4s.{HttpService, StaticFile, Status}
import kratia.kratia_community._
import kratia.kratia_configuration._
import kratia.kratia_protocol.{InMessage, OutMessage}
import kratia.kratia_protocol.OutMessage.{KratiaFailure, LogFromServer}
import kratia.utils.Log
import io.circe.generic.auto._
import kratia.kratia_core_model.Member
import kratia.members_events.MembersTopic
import kratia.members_store.MemberStore
import kratia.state.State
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebsocketBits.{Text, WebSocketFrame}
import io.circe.parser.parse
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object kratia_app {


  /** Models  */

  case class Kratia[F[_]](
    memberStore: MemberStore[F],
    membersTopic: MembersTopic[F],
    communities: Communities[F],
    timeStream: Stream[F, Time],
    scheduler: Scheduler,
    apiLogger: Log[F]
  )(implicit
    F: ConcurrentEffect[F],
    ec: ExecutionContext
  ) {

    val createMember: Pipe[F, String, Member] = Member.create[F](memberStore, membersTopic)
  }

  case class Time(value: Long) extends AnyVal


  /** Functions */

  def KratiaInMem[F[_]](implicit timer: Timer[F], F: ConcurrentEffect[F], ec: ExecutionContext): Stream[F, Kratia[F]] =
    for {
      config <- Stream.eval(loadConfig[F])
      memberStore <- Stream.eval(MemberStore.inMem[F])
      membersTopic <- Stream.eval(MembersTopic[F])
      communities <- Stream.eval(CommunitiesInMem[F])
      scheduler <- Scheduler[F](corePoolSize = 2)
      apiLogger = Log.colorPrint[F]("api")
    } yield Kratia[F](
      memberStore,
      membersTopic,
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

        implicit val showThrowable: Show[Throwable] =
          error => error.getMessage + "\n" + error.getStackTrace.mkString("\n")

        implicit val showJson: Show[Json] =
          json => json.spaces2

        val decodeMessage: Pipe[F, WebSocketFrame, InMessage] =
          _.flatMap {
              case Text((text, true)) =>
                Stream.emit(text)
              case _ =>
                Stream.raiseError[String](KratiaFailure(Status.BadRequest, "Messages need to be encoded in json format and fit in 1 frame."))
            }
            .map(parse)
            .flatMap {
              case Left(failure) =>
                Stream.eval(kratia.apiLogger.debug("Failed to parse message from client: " + failure)).flatMap { _ =>
                  Stream.raiseError[Json](KratiaFailure(Status.BadRequest, "Message is text but not json."))
                }
              case Right(json) =>
                Stream.emit(json)
            }
            .map(json => InMessage.decoder.apply(json.hcursor))
            .flatMap {
              case Left(failure) =>
                Stream.eval(kratia.apiLogger.debug("Failed to decode message from client: " + failure)).flatMap { _ =>
                  Stream.raiseError[InMessage](KratiaFailure(Status.BadRequest, "What are you talking about?"))
                }
              case Right(message) =>
                Stream.emit(message)
            }

        val encodeMessage: Pipe[F, OutMessage, WebSocketFrame] =
          _.map(OutMessage.encoder.apply)
            .map(json => Text(json.noSpaces))

        val handleRequest: Pipe[F, InMessage, OutMessage] =
          _.map(message => LogFromServer(message.toString))

        def errorHandler: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.handleErrorWith {
            case failure: KratiaFailure =>
              Stream.emit(failure)
                .map(failure => OutMessage.encoder(failure))
                .observe(kratia.apiLogger.error)
                .map(json => Text(json.noSpaces))
            case error =>
              Stream.emit(error)
                .observe(kratia.apiLogger.error)
                .map(_ => KratiaFailure(Status.InternalServerError, "Something went wrong on our side, please try again."))
                .map(failure => OutMessage.encoder(failure))
                .map(json => Text(json.noSpaces))
          }

        val requests: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.through(decodeMessage)
            .through(handleRequest)
            .through(encodeMessage)
            .through(errorHandler)

        for {
          queue <- fs2.async.unboundedQueue[F, WebSocketFrame]
          subscriptions <- State.inMem[F, String]
          send: Stream[F, WebSocketFrame] = queue.dequeue.through(requests)
          receive: Sink[F, WebSocketFrame] = queue.enqueue
          socket <- WebSocketBuilder[F].build(send, receive)
        } yield socket
    }
  }

  def KratiaMembersAPI[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ POST -> Root =>
        /*
        Stream
          .eval(request.as[String])
          .observe(kratia.apiLogger.infoNote("new member"))
          .through(kratia.createMember)
          .map(_.asJson)
          .evalMap(Ok(_))
          .compile
        .drain
        */
        Ok("Hello")
    }
  }

  private def timeStream[F[_]](config: KratiaConfig)(implicit timer: Timer[F], F: Sync[F]): Stream[F, Time] =
    Stream
      .duration[F]
      .evalMap(_ => timer.clockMonotonic(MILLISECONDS))
      .map(Time)
}
