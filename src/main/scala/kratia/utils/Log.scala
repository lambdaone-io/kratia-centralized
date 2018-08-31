package kratia.utils

import cats.effect.IO

trait Log[F[_]] {

  def info(message: String): F[Unit]

  def debug(message: String): F[Unit]

  def error(message: String): F[Unit]
}

object Log {

  def colorPrint: Log[IO] =
    new Log[IO] {

      override def info(message: String): IO[Unit] =
        IO(println(s"[${Console.GREEN}info${Console.RESET}] $message"))

      override def debug(message: String): IO[Unit] =
        IO(println(s"[${Console.MAGENTA}info${Console.RESET}] $message"))

      override def error(message: String): IO[Unit] =
        IO(println(s"[${Console.RED}info${Console.RESET}] $message"))
    }
}
