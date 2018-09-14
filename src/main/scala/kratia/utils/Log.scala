package kratia.utils

import cats.implicits._
import cats.Show
import cats.effect.{IO, Sync}
import fs2.Sink

trait Log[F[_]] {

  def info(message: String): F[Unit]

  def info[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(info)

  def infoNote[A: Show](note: String): Sink[F, A] =
    _.map(_.show).map(a => s"<$a> $note").evalMap(info)

  def debug(message: String): F[Unit]

  def debug[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(debug)

  def error(message: String): F[Unit]

  def error[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(error)
}

object Log {

  def colorPrint[F[_]](name: String)(implicit F: Sync[F]): Log[F] =
    new Log[F] {

      override def info(message: String): F[Unit] =
        F.delay(println(s"[${Console.GREEN}$name${Console.RESET}] $message"))

      override def debug(message: String): F[Unit] =
        F.delay(println(s"[${Console.MAGENTA}$name${Console.RESET}] $message"))

      override def error(message: String): F[Unit] =
        F.delay(println(s"[${Console.RED}$name${Console.RESET}] $message"))
    }
}
