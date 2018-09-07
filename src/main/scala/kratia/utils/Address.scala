package kratia.utils

import java.util.UUID

import cats.effect.Sync

case class Address(value: UUID) extends AnyVal

object Address {

  def genAddress[F[_]](implicit sync: Sync[F]): F[Address] =
    sync.delay(Address(UUID.randomUUID()))
}
