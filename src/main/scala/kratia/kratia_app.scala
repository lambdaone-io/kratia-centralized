package kratia

import fs2.Stream
import cats.implicits._
import cats.effect.{Concurrent, ConcurrentEffect, Timer}
import org.http4s.implicits._
import org.http4s.{HttpRoutes, StaticFile, Status}
import kratia.kratia_member._
import kratia.kratia_community._
import kratia.kratia_configuration._
import org.http4s.dsl.Http4sDsl

import scala.concurrent.duration._

object kratia_app {


  /** Models  */

  case class Kratia[F[_]](members: Members[F], communities: Communities[F], timeStream: Stream[F, Time])

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
    } yield Kratia[F](members, communities, timeStream[F](config))

  def KratiaStaticFiles[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: Concurrent[F]): HttpRoutes[F] = {
    import dsl._
    HttpRoutes.of[F] {

      case request @ GET -> Root =>
        StaticFile.fromResource("/static/index.html", Some(request)).getOrElseF(NotFound())

      case request @ GET -> "static" /: path =>
        StaticFile.fromResource("/static" + path.toString, Some(request)).getOrElseF(NotFound())
    }
  }

  def KratiaAPI[F[_]](kratia: Kratia[F], dsl: Http4sDsl[F])(implicit F: Concurrent[F]): HttpRoutes[F] = {
    import dsl._
    HttpRoutes.of[F] {

      case request @ GET -> Root =>
        Ok("pong")
    }
  }

  private def timeStream[F[_]](config: KratiaConfig)(implicit timer: Timer[F]): Stream[F, Time] =
    Stream
      .fixedRate(config.appSpeed)
      .evalMap(_ => timer.clockMonotonic(MILLISECONDS))
      .map(Time)
}
