package lambdaone.toolbox.denotations

import cats.data.State
import lambdaone.toolbox.UniqueGen
import UniqueGenDenotation._

object UniqueGenDenotation {

  /** Invariant: every member of the LastGenerated is unique */
  type LastGenerated[A] = List[A]

  /** Invariant: every A outcome of Generator is not in the domain List[A]
    *
    * \-/ xs \in List[A] | Generator(xs) \notin xs
    */
  type Generator[A] = List[A] => A

  type Denotation[A, T] = State[(LastGenerated[A], Generator[A]), T]
}

class UniqueGenDenotation[A] extends UniqueGen[Denotation[A, ?], A] {

  /**
    * Generates a unique A every time `gen` is called
    */
  override def gen: Denotation[A, A] =
    State { case (state, generator) => (generator(state) :: state, generator) -> generator(state) }
}
