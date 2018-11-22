package lambdaone.toolbox.denotations

import cats.data.{Kleisli, State}
import lambdaone.toolbox.UniqueGen

case class UniqueGenDenotation[A]() extends UniqueGen[Kleisli[State[A, ?], A => A, ?], A] {

  /**
    * Generates a unique A every time `gen` is called
    */
  override def gen: Kleisli[State[A, ?], A => A, A] =
    Kleisli.ask[State[A, ?], A => A].flatMapF(f => State[A, A] { last => (f(last), f(last)) })
}
