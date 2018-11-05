package lambdaone.toolbox

import java.util.UUID

import cats.effect.Sync

/** Generates a unique A every time `gen` is called */
trait UniqueGen[F[_], A] {

  def gen: F[A]
}

object UniqueGen {

  case class UniqueInt(value: Int) extends AnyVal

  implicit def uniqueGenReference[F[_]](implicit state: StateMonad[F, UniqueInt]): UniqueGen[F, UniqueInt] =
    new UniqueGen[F, UniqueInt] {
      override def gen: F[UniqueInt] =
        state.run(x => (UniqueInt(x.value + 1), x))
    }

  implicit def uniqueGenUUID[F[_]](implicit F: Sync[F]): UniqueGen[F, UUID] =
    new UniqueGen[F, UUID] {
      override def gen: F[UUID] = F.delay(UUID.randomUUID())
    }
}
