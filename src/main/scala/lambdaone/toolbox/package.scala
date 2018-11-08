package lambdaone

import cats.effect.IO
import cats.{Id, ~>}

package object toolbox {

  type |->[A, B] = Map[A, B]

  object IOInterpreter extends (IO ~> Id) {
    override def apply[A](fa: IO[A]): Id[A] = fa.unsafeRunSync()
  }
}
