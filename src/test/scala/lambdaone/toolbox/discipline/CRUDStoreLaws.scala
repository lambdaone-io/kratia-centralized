package lambdaone.toolbox.discipline

import lambdaone.helpers._
import cats.{Id, Monad, Monoid, ~>}
import cats.implicits._
import lambdaone.helpers.IsEq
import lambdaone.toolbox.CRUDStore

trait CRUDStoreLaws[F[_], I, A] {

  def crud: CRUDStore[F, I, A]

  def interpret: F ~> Id

  def fetching(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] =
      xs.traverse(crud.create) >>= (_.traverse(crud.get))
    interpret(program).sorted <-> xs.sorted.map(Option.apply)
  }

  def creation(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A]): IsEq[List[A]] = {
    val program: F[List[A]] =
      xs.traverse(crud.create) *> crud.all
    interpret(program).sorted <-> xs.sorted
  }

  def deletion(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] =
      xs.traverse(crud.create) >>= (_.traverse(crud.delete))
    interpret(program).sorted <-> xs.sorted.map(Option.apply)
  }

  def updates(xs: List[A])(implicit F: Monad[F], ordering: Ordering[A], monoid: Monoid[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] =
      xs.traverse(crud.create) >>= (_.traverse(crud.update(_)(a => a |+| a)))
    interpret(program).sorted <-> xs.sorted.map(a => Option(a |+| a))
  }
}

object CRUDStoreLaws {

  def apply[F[_], I, A](implicit store: CRUDStore[F, I, A], run: F ~> Id): CRUDStoreLaws[F, I, A] =
    new CRUDStoreLaws[F, I, A] {

      override def crud: CRUDStore[F, I, A] = store

      override def interpret: F ~> Id = run
    }
}
