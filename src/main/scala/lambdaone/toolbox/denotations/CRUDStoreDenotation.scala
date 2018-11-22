package lambdaone.toolbox.denotations

import cats.data.State
import lambdaone.toolbox.CRUDStore
import CRUDStoreDenotation._
import cats.arrow.FunctionK
import cats.{Id, ~>}

object CRUDStoreDenotation {

  def apply[I, A]: CRUDStoreDenotation[I, A] =
    new CRUDStoreDenotation[I, A] {}

  type Denotation[I, A, T] = State[Map[I, A], T]

  def run[I, A](initial: Map[I, A]): Denotation[I, A, ?] ~> Id =
    new FunctionK[Denotation[I, A, ?], Id] {
      def apply[T](fa: Denotation[I, A, T]): Id[T] =
        fa.run(initial).value._2
    }

}

trait CRUDStoreDenotation[I, A] extends CRUDStore[Denotation[I, A, ?], I, A] {

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): Denotation[I, A, Option[A]] =
    State.inspect { s => s.get(id) }

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): Denotation[I, A, Option[A]] =
    State { s =>
      s.get(id) match {
        case None => s -> None
        case Some(a) => (s + (id -> a)) -> Some(f(a))
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): Denotation[I, A, Option[A]] =
    State { s =>
      s.get(id) match {
        case None => s -> None
        case Some(a) => (s - id) -> Some(a)
      }
    }

  /** Returns all data within the store */
  override def all: Denotation[I, A, Map[I, A]] =
    State.inspect { identity }

}
