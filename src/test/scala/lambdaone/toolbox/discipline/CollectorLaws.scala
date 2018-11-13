package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Id, Monad, ~>}
import lambdaone.collector.Collector
import lambdaone.helpers.{IsEq, _}

trait CollectorLaws[F[_], I, A] {

  def coll: Collector[F, I, A]

  def interpret: F ~> Id

  def creating(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] =
      xs.traverse(coll.create) >>= (_.traverse(coll.get))
    interpret(program).sorted <-> xs.sorted.map(Option.apply)
  }

}


object CollectorLaws {

  def apply[F[_], I, A](implicit collector: Collector[F, I, A], run: F ~> Id): CollectorLaws[F, I, A] =
    new CollectorLaws[F, I, A] {

      override def coll: Collector[F, I, A] = collector

      override def interpret: F ~> Id = run
    }
}


