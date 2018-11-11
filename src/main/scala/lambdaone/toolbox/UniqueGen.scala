package lambdaone.toolbox

import java.util.UUID

import cats.effect.Sync

trait UniqueGen[F[_], A] {

  /**
    * Generates a unique A every time `gen` is called
    */
  def gen: F[A]

}

object UniqueGen {

  def apply[F[_], A](implicit uniqueGen: UniqueGen[F, A]): UniqueGen[F, A] = uniqueGen

  implicit def UniqueGenUUID[F[_]](implicit F: Sync[F]): UniqueGen[F, UUID] =
    new UniqueGen[F, UUID] {
      override def gen: F[UUID] = F.delay(UUID.randomUUID())
    }

}
