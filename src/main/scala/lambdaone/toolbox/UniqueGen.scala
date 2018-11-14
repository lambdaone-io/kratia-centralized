package lambdaone.toolbox

import java.util.UUID

import cats.{Invariant, Semigroupal}
import cats.implicits._
import cats.effect.{IO, Sync}

trait UniqueGen[F[_], A] {

  /**
    * Generates a unique A every time `gen` is called
    */
  def gen: F[A]

}

object UniqueGen {

  def apply[F[_], A](implicit uniqueGen: UniqueGen[F, A]): UniqueGen[F, A] = uniqueGen

  def lift[F[_], A](fa: F[A]): UniqueGen[F, A] =
    new UniqueGen[F, A] { def gen: F[A] = fa }

  implicit def UniqueGenUUID[F[_]](implicit F: Sync[F]): UniqueGen[F, UUID] =
    lift(F.delay(UUID.randomUUID()))

  implicit def

  implicit def UniqueGenTupled[F[_], A](implicit other: UniqueGen[F, A], semi: Semigroupal[F], invariant: Invariant[F]): UniqueGen[F, (A, A)] =
    new Un
}
