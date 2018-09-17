package kratia.utils

import cats.implicits._
import cats.Show
import cats.effect.Sync
import fs2.Sink

sealed trait Logger[F[_]] {

  def info(message: String): F[Unit]

  def info[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(info)

  def infoNote[A: Show](note: String): Sink[F, A] =
    _.map(_.show).map(a => s"<$a> $note").evalMap(info)

  def debug(message: String): F[Unit]

  def debugSink[A](message: String): Sink[F, A] =
    _.evalMap(_ => debug(message))

  def debug[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(debug)

  def error(message: String): F[Unit]

  def error[A: Show]: Sink[F, A] =
    _.map(_.show).evalMap(error)
}

object Logger {

  case class ColorPrint[F[_]](name: String)(implicit F: Sync[F]) extends Logger[F] {

    override def info(message: String): F[Unit] =
      F.delay(println(s"[${Console.GREEN}$name${Console.RESET}] $message"))

    override def debug(message: String): F[Unit] =
      F.delay(println(s"[${Console.MAGENTA}$name${Console.RESET}] $message"))

    override def error(message: String): F[Unit] =
      F.delay(println(s"[${Console.RED}$name${Console.RESET}] $message"))
  }
}
