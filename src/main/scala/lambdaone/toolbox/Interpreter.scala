package lambdaone.toolbox

import cats.~>

import scala.annotation.implicitNotFound

@implicitNotFound("Couldn't find an algebra ${Algebra} of language ${Language} that can be interpreted as ${F}.")
trait Interpreter[Algebra[_[_]], Language[_], F[_]] {

  def algebra: Algebra[Language]

  def interpret: Language ~> F

  def run[A](f: Algebra[Language] => Language[A]): F[A] =
    interpret(f(algebra))
}

object Interpreter {

  def apply[Algebra[_[_]], Language[_], F[_]](implicit interpreter: Interpreter[Algebra, Language, F]): Interpreter[Algebra, Language, F] = interpreter

  def of[Algebra[_[_]], Language[_], F[_]](
    algebra0: Algebra[Language],
    interpret0: Language ~> F
  ): Interpreter[Algebra, Language, F] =
    new Interpreter[Algebra, Language, F] {
      override val algebra: Algebra[Language] = algebra0
      override val interpret: ~>[Language, F] = interpret0
    }
}
