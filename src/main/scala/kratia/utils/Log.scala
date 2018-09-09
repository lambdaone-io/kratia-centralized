package kratia.utils

import cats.effect.{IO, Sync}

trait Log[F[_]] {

  def info(message: String): F[Unit]

  def debug(message: String): F[Unit]

  def error(message: String): F[Unit]
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
