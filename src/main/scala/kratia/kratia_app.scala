package kratia

import fs2.Stream
import cats.implicits._
import cats.effect.{Concurrent, ConcurrentEffect, Sync, Timer}
import org.http4s.{HttpService, StaticFile, Status}
import kratia.kratia_member._
import kratia.kratia_community._
import kratia.kratia_configuration._
import kratia.utils.Log
import org.http4s.dsl.Http4sDsl

import scala.concurrent.duration._

object kratia_app {


  /** Models  */

  case class Kratia[F[_]](
    members: Members[F],
    communities: Communities[F],
    timeStream: Stream[F, Time],
    apiLogger: Log[F]
  )

  trait KratiaFailure extends Throwable {

    def code: Status

    def message: String
  }

  case class Time(value: Long) extends AnyVal


  /** Functions */

  def KratiaInMem[F[_]](implicit timer: Timer[F], F: ConcurrentEffect[F]): F[Kratia[F]] =
    for {
      config <- loadConfig[F]
      members <- MembersInMem[F]
      communities <- CommunitiesInMem[F]
      apiLogger = Log.colorPrint[F]
    } yield Kratia[F](
      members,
      communities,
      timeStream[F](config),
      apiLogger
    )

  def KratiaStaticFiles[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: Concurrent[F]): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ GET -> Root =>
        StaticFile.fromResource("/static/index.html", Some(request)).getOrElseF(NotFound())

      case request @ GET -> "static" /: path =>
        StaticFile.fromResource("/static" + path.toString, Some(request)).getOrElseF(NotFound())
    }
  }

  def KratiaAPI[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: Concurrent[F]): HttpService[F] = {
    import dsl._
    HttpService[F] {

      case request @ POST -> Root / "member" =>
        request.as[String]
          .flatMap(MemberInMem(_)(kratia.members))
          .flatTap(member => kratia.apiLogger.info("Registered new member: " + member.nickname))
          .flatMap(member => Ok(member.secret.value.toString))
    }
  }

  private def timeStream[F[_]](config: KratiaConfig)(implicit timer: Timer[F], F: Sync[F]): Stream[F, Time] =
    Stream
      .duration[F]
      .evalMap(_ => timer.clockMonotonic(MILLISECONDS))
      .map(Time)
}
