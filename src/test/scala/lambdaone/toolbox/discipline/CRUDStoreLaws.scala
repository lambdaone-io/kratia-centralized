package lambdaone.toolbox.discipline

import lambdaone.helpers._
import cats.{Id, Monad, Monoid, ~>}
import cats.implicits._
import lambdaone.helpers.IsEq
import lambdaone.toolbox.CRUDStore

trait CRUDStoreLaws[F[_], D[_], I, A] {

  implicit def implementation: CRUDStore[F, I, A]

  implicit def denotation: CRUDStore[D, I, A]

  implicit val F: Monad[F]

  implicit val D: Monad[D]

  implicit val ordering: Ordering[A]

  implicit val monoid: Monoid[A]

  def implement: F ~> Id

  def denote: D ~> Id

  def fetching(xs: List[A]): IsEq[List[Option[A]]] = {

    def program[G[_]](implicit G: Monad[G], cat: CRUDStore[G, I, A]): G[List[Option[A]]] =
      xs.traverse(cat.create) >>= (_.traverse(cat.get))

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }

  def creation(xs: List[A]): IsEq[List[A]] = {

    def program[G[_]](implicit G: Monad[G], cat: CRUDStore[G, I, A]): G[List[A]] =
      xs.traverse(cat.create) *> cat.all.map(_.values.toList)

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }

  def deletion(xs: List[A]): IsEq[List[Option[A]]] = {

    def program[G[_]](implicit G: Monad[G], cat: CRUDStore[G, I, A]): G[List[Option[A]]] =
      xs.traverse(cat.create) >>= (_.traverse(cat.delete))

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }

  def updates(xs: List[A]): IsEq[List[Option[A]]] = {

    def program[G[_]](implicit G: Monad[G], cat: CRUDStore[G, I, A]): G[List[Option[A]]] =
      xs.traverse(cat.create) >>= (_.traverse(cat.update(_)(a => a |+| a)))

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }

  def creationPick(a: A, id: I): IsEq[List[A]] = {

    def program[G[_]](implicit G: Monad[G], cat: CRUDStore[G, I, A]): G[List[A]] =
      cat.createPick(a, id) *> cat.createPick(a, id) *> cat.all.map(_.values.toList)

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }
}

object CRUDStoreLaws {

  def apply[F[_], D[_], I, A](implicit
    implementation0: CRUDStore[F, I, A],
    denotation0: CRUDStore[D, I, A],
    F0: Monad[F],
    D0: Monad[D],
    ordering0: Ordering[A],
    monoid0: Monoid[A],
    interpret0: F ~> Id,
    denote0: D ~> Id
  ): CRUDStoreLaws[F, D, I, A] =
    new CRUDStoreLaws[F, D, I, A] {
      def implementation: CRUDStore[F, I, A] = implementation0
      def denotation: CRUDStore[D, I, A] = denotation0
      implicit val F: Monad[F] = F0
      implicit val D: Monad[D] = D0
      implicit val ordering: Ordering[A] = ordering0
      implicit val monoid: Monoid[A] = monoid0
      def implement: F ~> Id = interpret0
      def denote: D ~> Id = denote0
    }
}
