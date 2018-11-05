package lambdaone.toolbox

import cats.Monad
import cats.data.State

trait StateMonad[F[_], S] extends Monad[F] {

  def run[A](f: S => (S, A)): F[A]

  /**
   * Return `a` and maintain the input state.
   */
  def pure[A](a: A): F[A] = run(s => (s, a))

  /**
   * Modify the input state and return Unit.
   */
  def modify(f: S => S): F[Unit] = run(s => (f(s), ()))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[A](f: S => A): F[A] = run(s => (s, f(s)))

  /**
   * Return the input state without modifying it.
   */
  def get: F[S] = inspect(identity)

  /**
   * Set the state to `s` and return Unit.
   */
  def set(s: S): F[Unit] = run(_ => (s, ()))
}

object StateMonad {

  implicit def referenceStateMonad[S]: StateMonad[State[S, ?], S] =
    new StateMonad[State[S, ?], S] {

      override def run[A](f: S => (S, A)): State[S, A] =
        State(f)

      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] =
        Monad[State[S, ?]].tailRecM(a)(f)
    }

}