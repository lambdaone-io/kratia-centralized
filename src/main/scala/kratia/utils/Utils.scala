package kratia.utils

import java.util.UUID

import cats.effect.Sync

object Utils {

  case class Address(value: UUID) extends AnyVal

  def genAddress[F[_]](implicit sync: Sync[F]): F[Address] =
    sync.delay(Address(UUID.randomUUID()))
}
