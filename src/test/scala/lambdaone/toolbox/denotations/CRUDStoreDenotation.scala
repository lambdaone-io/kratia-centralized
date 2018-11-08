package lambdaone.toolbox.denotations

import cats.data.State
import lambdaone.toolbox.CRUDStore
import CRUDStoreDenotation._
import cats.{Id, ~>}

object CRUDStoreDenotation {

  type LastIds[I] = List[I]

  type IdGenerator[I] = List[I] => I

  type LastStorage[I, A] = Map[I, A]

  type Denotation[I, A, T] = State[(LastStorage[I, A], LastIds[I], IdGenerator[I]), T]

  def numericGenerator[I](implicit numeric: Numeric[I]): IdGenerator[I] = {
    case Nil => numeric.zero
    case x :: _ => numeric.plus(x, numeric.one)
  }

  def Interpreter[I, A](implicit numeric: Numeric[I]): Denotation[I, A, ?] ~> Id =
    new (Denotation[I, A, ?] ~> Id) {
      override def apply[T](fa: Denotation[I, A, T]): Id[T] =
        fa.runA((Map.empty, List.empty, numericGenerator)).value
    }

  def apply[I, A]: CRUDStoreDenotation[I, A] = new CRUDStoreDenotation()
}

class CRUDStoreDenotation[I, A] extends CRUDStore[Denotation[I, A, ?], I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): Denotation[I, A, I] = {

    def generateId: Denotation[I, A, I] =
      State { case (s, i, g) => (s, g(i) :: i, g) -> g(i) }

    def addToState(newId: I): Denotation[I, A, Unit] =
      State.modify { case (s, i, g) => (s + (newId -> a), newId :: i, g) }

    for {
      newId <- generateId
      _ <- addToState(newId)
    } yield newId
  }

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def createPick(a: A, id: I): Denotation[I, A, Boolean] = {
    State { case (s, i, g) =>
      if (s.contains(id)) (s, i, g) -> false
      else (s + (id -> a), id :: i, g) -> true
    }
  }

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): Denotation[I, A, Option[A]] =
    State.inspect { case (s, _, _) => s.get(id) }

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): Denotation[I, A, Option[A]] =
    State { case (s, i, g) =>
      s.get(id) match {
        case None => (s, i, g) -> None
        case Some(a) => (s + (id -> a), i, g) -> Some(f(a))
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): Denotation[I, A, Option[A]] =
    State { case (s, i, g) =>
      s.get(id) match {
        case None => (s, i, g) -> None
        case Some(a) => (s - id, i, g) -> Some(a)
      }
    }

  /** Returns all data within the store */
  override def all: Denotation[I, A, List[A]] =
    State.inspect { case (s, _, _) => s.values.toList }
}
