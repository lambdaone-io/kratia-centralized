package lambdaone.toolbox

import java.util.UUID

import cats.data.State
import cats.{Functor, Invariant, Semigroupal}
import cats.implicits._
import cats.effect.Sync

trait UniqueGen[F[_], A] { self =>

  /**
    * Generates a unique A every time `gen` is called
    */
  def gen: F[A]

  def map[B](f: A => B)(implicit F: Functor[F]): UniqueGen[F, B] =
    new UniqueGen[F, B] { def gen: F[B] = self.gen.map(f) }
}

object UniqueGen {

  def apply[F[_], A](implicit uniqueGen: UniqueGen[F, A]): UniqueGen[F, A] = uniqueGen

  def lift[F[_], A](fa: F[A]): UniqueGen[F, A] =
    new UniqueGen[F, A] { def gen: F[A] = fa }

  implicit def UniqueGenDenotation: UniqueGen[State[Int, ?], Int] =
    new UniqueGen[State[Int, ?], Int] {
      def gen: State[Int, Int] =
        State[Int, Int] { last =>
          val int = last + 1
          (int, int)
        }
    }

  implicit def UniqueGenUUID[F[_]](implicit F: Sync[F]): UniqueGen[F, UUID] =
    lift(F.delay(UUID.randomUUID()))

  implicit def UniqueGenTupled[F[_], A](implicit other: UniqueGen[F, A], semi: Semigroupal[F], invariant: Invariant[F]): UniqueGen[F, (A, A)] =
    new UniqueGen[F, (A, A)] { def gen: F[(A, A)] = (other.gen, other.gen).tupled }
}
