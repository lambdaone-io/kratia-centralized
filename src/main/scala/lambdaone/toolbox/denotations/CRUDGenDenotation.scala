package lambdaone.toolbox.denotations

import cats.{Id, ~>}
import cats.arrow.FunctionK
import cats.data.{Kleisli, State}
import lambdaone.toolbox.CRUDGen
import lambdaone.toolbox.denotations.CRUDGenDenotation.{Denotation, StoreState}

object CRUDGenDenotation {

  type Generator[I] = I => I

  type GeneratorState[I] = I

  type StoreState[I, A] = (Map[I, A], GeneratorState[I])

  type Denotation[I, A, T] = Kleisli[State[StoreState[I, A], ?], Generator[I], T]

  def run[I, A](initial: (Map[I, A], I), generator: I => I): Denotation[I, A, ?] ~> Id =
    new FunctionK[Denotation[I, A, ?], Id] {
      def apply[T](fa: Denotation[I, A, T]): Id[T] =
        fa.run(generator).run(initial).value._2
    }

}

case class CRUDGenDenotation[I, A]() extends CRUDGen[Denotation[I, A, ?], I, A] {

  private val gen: UniqueGenDenotation[I] = UniqueGenDenotation()

  private val crud: CRUDStoreDenotation[I, A] = CRUDStoreDenotation[I, A]

  private def modify(f: Map[I, A] => Map[I, A]): Denotation[I, A, Unit] =
    state(s => (f(s), ()))

  private def inspect[T](f: Map[I, A] => T): Denotation[I, A, T] =
    state(s => (s, f(s)))

  private def state[T](f: Map[I, A] => (Map[I, A], T)): Denotation[I, A, T] =
    Kleisli.liftF { State { case (s, i) =>
      val (ns, t) = f(s)
      (ns, i) -> t
    } }

  private def adapt[T](fa: CRUDStoreDenotation.Denotation[I, A, T]): Denotation[I, A, T] =
    Kleisli.liftF { fa.transformS[StoreState[I, A]](_._1, (t, i) => (i, t._2)) }

  private def generateId: Denotation[I, A, I] =
    gen.gen.mapF {
      _.transformS(
        _._2,
        (r, sb) => (r._1, sb)
      )
    }

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): Denotation[I, A, I] = {
    for {
      newId <- generateId
      _ <- modify { s => s + (newId -> a) }
    } yield newId
  }

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): Denotation[I, A, Option[A]] =
    adapt(crud.get(id))

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): Denotation[I, A, Option[A]] =
    adapt(crud.update(id)(f))

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): Denotation[I, A, Option[A]] =
    adapt(crud.delete(id))

  /** Returns all data within the store */
  override def all: Denotation[I, A, Map[I, A]] =
    adapt(crud.all)
}
