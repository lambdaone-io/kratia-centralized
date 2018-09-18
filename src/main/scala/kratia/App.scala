package kratia

import cats.Show
import fs2.{Pipe, Scheduler, Sink, Stream}
import cats.implicits._
import cats.effect.{ConcurrentEffect, Sync, Timer}
import org.http4s.{HttpService, StaticFile, Status}
import kratia.Configuration._
import kratia.Protocol.{InMessage, KrRequest, OutMessage, ProtocolMessage}
import kratia.utils.{Interrupt, KratiaChannels, Logger, State}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebsocketBits.{Text, WebSocketFrame}
import io.circe.parser.parse
import io.circe.Json
import kratia.Protocol.ProtocolMessage.{KratiaFailure, LogFromServer, Register, Registered, Subscribe, Unsubscribe}
import kratia.communities.CommunitiesService
import kratia.members.MemberService
import kratia.utils.Logger.ColorPrint

import scala.concurrent.ExecutionContext

object App {

  /** Models  */

  case class Kratia[F[_]](
    channels: KratiaChannels[F],
    members: MemberService[F],
    communities: CommunitiesService[F],
    scheduler: Scheduler,
    log: Logger[F]
  )

  case class ClientConnection[F[_]](
    subscriptions: State[F, Map[String, Interrupt[F]]],
    redirectQueue: fs2.async.mutable.Queue[F, OutMessage],
    requestQueue: fs2.async.mutable.Queue[F, KrRequest]
  ) {

    def redirect(out: OutMessage): F[Unit] =
      redirectQueue.enqueue1(out)

    def request(request: KrRequest): F[Unit] =
      requestQueue.enqueue1(request)
  }

  case class Connection[F[_]](send: Stream[F, OutMessage], receive: Sink[F, ProtocolMessage])

  implicit val showThrowable: Show[Throwable] =
    error => error.getMessage + "\n" + error.getStackTrace.mkString("\n")

  implicit val showJson: Show[Json] =
    json => json.spaces2


  /** Functions */

  def KratiaInMem[F[_]](implicit timer: Timer[F], F: ConcurrentEffect[F], ec: ExecutionContext): Stream[F, Kratia[F]] =
    for {
      config <- Stream.eval(loadConfig[F])
      apiLogger = ColorPrint[F]("kratia")
      channels <- Stream.eval(KratiaChannels.inMem[F](apiLogger))
      memberService <- Stream.eval(MemberService.inMem[F](channels))
      communitiesService <- Stream.eval(CommunitiesService.inMem[F](channels))
      scheduler <- Scheduler[F](corePoolSize = 2)
    } yield Kratia[F](
      channels,
      memberService,
      communitiesService,
      scheduler,
      apiLogger
    )

  def KratiaStaticFiles[F[_]](dsl: Http4sDsl[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ GET -> Root =>
        StaticFile.fromResource("/static/index.html", Some(request)).getOrElseF(NotFound())

      case request @ GET -> "static" /: path =>
        StaticFile.fromResource("/static" + path.toString, Some(request)).getOrElseF(NotFound())
    }
  }

  def KratiaBroker[F[_]](dsl: Http4sDsl[F], kratia: Kratia[F])(implicit F: ConcurrentEffect[F], ec: ExecutionContext): HttpService[F] = {
    import dsl._
    implicit val app: Kratia[F] = kratia
    HttpService[F] {
      case GET -> Root / "broker" =>
        for {
          connection <- connect[F]
          send = connection.send.through(encodeMessage)
          receive = (in: Stream[F, WebSocketFrame]) => in.through(decodeMessage).to(connection.receive)
          socket <- WebSocketBuilder[F].build(send, receive)
        } yield socket
    }
  }

  def connect[F[_]](implicit kratia: Kratia[F], F: ConcurrentEffect[F], ec: ExecutionContext): F[Connection[F]] = {
    for {
      requestsQueue <- fs2.async.unboundedQueue[F, KrRequest]
      redirectQueue <- fs2.async.unboundedQueue[F, OutMessage]
      subscriptions <- State.inMem[F, Map[String, Interrupt[F]]](Map.empty)
      feed: ClientConnection[F] = ClientConnection[F](subscriptions, redirectQueue, requestsQueue)
      send: Stream[F, OutMessage] = requestsQueue
        .dequeue.through(handleRequest)
        .merge(redirectQueue.dequeue)
        .through(errorHandler)
      receive: Sink[F, ProtocolMessage] = handleInMessages(feed)
    } yield Connection[F](send, receive)
  }

  def handleRequest[F[_]](implicit kratia: Kratia[F], F: ConcurrentEffect[F], ec: ExecutionContext): Pipe[F, KrRequest, OutMessage] =
    _.evalMap {
      case Register(nickname) =>
        kratia.members.create(nickname).map[OutMessage](member => Registered(member))
      case message =>
        kratia.log.info("Got: " + message)
          .map[OutMessage](_ => LogFromServer(message.toString))
    }

  def handleInMessages[F[_]](feed: ClientConnection[F])(implicit kratia: Kratia[F], F: ConcurrentEffect[F], ec: ExecutionContext): Sink[F, ProtocolMessage] =
    _.evalMap {
        case Subscribe(topic) =>
          kratia.channels.subscribe(topic, feed)
        case Unsubscribe(topic) =>
          kratia.channels.unsubscribe(topic, feed)
        case request: KrRequest =>
          feed.request(request)
        case in: InMessage =>
          kratia.log.debug("Unsupported protocol message: " + in) *>
            feed.redirect(LogFromServer("Unsupported protocol message :" + in))
        case other =>
          kratia.log.debug("Received message out of protocol: " + other)
      }

  def errorHandler[F[_]](implicit kratia: Kratia[F], F: ConcurrentEffect[F], ec: ExecutionContext): Pipe[F, OutMessage, OutMessage] =
    _.handleErrorWith {
      case failure: KratiaFailure =>
        Stream.emit(failure)
        /*
          .map(failure => ProtocolMessage.encoder(failure))
          .observe(kratia.log.error)
          .map(json => Text(json.noSpaces))
        */
      case error =>
        Stream.emit(error)
          .observe(kratia.log.error)
          .map(_ => KratiaFailure(Status.InternalServerError.code, "Something went wrong on our side, please try again."))
    }

  def decodeMessage[F[_]](implicit kratia: Kratia[F], F: Sync[F]): Pipe[F, WebSocketFrame, ProtocolMessage] =
    _.flatMap {
      case Text((text, true)) =>
        Stream.emit(text)
      case _ =>
        Stream.raiseError[String](KratiaFailure(Status.BadRequest.code, "Messages need to be encoded in json format and fit in 1 frame."))
    }
      .map(parse)
      .flatMap {
        case Left(failure) =>
          Stream.eval(kratia.log.debug("Failed to parse message from client: " + failure)).flatMap { _ =>
            Stream.raiseError[Json](KratiaFailure(Status.BadRequest.code, "Message is text but not json."))
          }
        case Right(json) =>
          Stream.emit(json)
      }
      .map(json => ProtocolMessage.decoder.apply(json.hcursor))
      .flatMap {
        case Left(failure) =>
          Stream.eval(kratia.log.debug("Failed to decode message from client: " + failure)).flatMap { _ =>
            Stream.raiseError[InMessage](KratiaFailure(Status.BadRequest.code, "What are you talking about?"))
          }
        case Right(message) =>
          Stream.emit(message)
      }

  def encodeMessage[F[_]](implicit F: Sync[F]): Pipe[F, ProtocolMessage, WebSocketFrame] =
    _.map(ProtocolMessage.encoder.apply)
      .map(json => Text(json.noSpaces))
}
