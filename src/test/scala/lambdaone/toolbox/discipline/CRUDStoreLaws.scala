package lambdaone.toolbox.discipline

import lambdaone.helpers._
import cats.{Id, Monad, Monoid, ~>}
import cats.implicits._
import lambdaone.helpers.IsEq
import lambdaone.toolbox.CRUDStore

trait CRUDStoreLaws[F[_], I, A] {

  def crud: CRUDStore[F, I, A]

  def interpret: F ~> Id

  def creation(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A]): IsEq[List[A]] = {
    val program: F[List[A]] =
      xs.traverse(crud.create) *> crud.all.map(_.toList)
    interpret(program).sorted <-> xs.sorted
  }

  def deletion(a: A)(implicit F: Monad[F]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] = for {
      id <- crud.create(a)
      optA0 <- crud.get(id)
      optA1 <- crud.delete(id)
      all <- crud.all
    } yield List(optA0, optA1) ++ all.map(Option.apply)
    interpret(program) <-> List(Some(a), Some(a))
  }

  def nonDestructiveUpdates(xs: List[A])(implicit F: Monad[F], monoid: Monoid[A], ordering: Ordering[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] = for {
      ids <- xs.traverse(crud.create)
      updates <- ids.traverse(crud.update(_)(_ |+| monoid.empty))
    } yield updates
    interpret(program).sorted <-> xs.map(Option.apply).sorted
  }
}

object CRUDStoreLaws {

  def apply[F[_], I, A](implicit store: CRUDStore[F, I, A], run: F ~> Id): CRUDStoreLaws[F, I, A] =
    new CRUDStoreLaws[F, I, A] {

      override def crud: CRUDStore[F, I, A] = store

      override def interpret: F ~> Id = run
    }
}
