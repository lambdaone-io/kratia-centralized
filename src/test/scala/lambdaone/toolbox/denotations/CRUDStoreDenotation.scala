package lambdaone.toolbox.denotations

import cats.data.State
import lambdaone.toolbox.{CRUDStore, UniqueGen}
import CRUDStoreDenotation._
import cats.arrow.FunctionK
import cats.{Id, ~>}

object CRUDStoreDenotation {

  type Denotation[I, A, T] = State[(Map[I, A], I), T]

  def run[I, A](initial: (Map[I, A], I)): Denotation[I, A, ?] ~> Id =
    FunctionK.lift[Denotation[I, A, ?], Id] {
      _.run(initial).value
    }

}

case class CRUDStoreDenotation[I, A](gen: UniqueGen[State[I, ?], I]) extends CRUDStore[Denotation[I, A, ?], I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): Denotation[I, A, I] = {

    def addToState(newId: I): Denotation[I, A, Unit] =
      State.modify { case (s, i) => (s + (newId -> a), i) }

    for {
      newId <- gen.gen.contramap[(Map[I, A], I)](_._2)
      _ <- addToState(newId)
    } yield newId
  }

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def createPick(a: A, id: I): Denotation[I, A, Boolean] = {
    State { case (s, i) =>
      if (s.contains(id)) (s, i) -> false
      else (s + (id -> a), i) -> true
    }
  }

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): Denotation[I, A, Option[A]] =
    State.inspect { case (s, _) => s.get(id) }

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): Denotation[I, A, Option[A]] =
    State { case (s, i) =>
      s.get(id) match {
        case None => (s, i) -> None
        case Some(a) => (s + (id -> a), i) -> Some(f(a))
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): Denotation[I, A, Option[A]] =
    State { case (s, i) =>
      s.get(id) match {
        case None => (s, i) -> None
        case Some(a) => (s - id, i) -> Some(a)
      }
    }

  /** Returns all data within the store */
  override def all: Denotation[I, A, List[A]] =
    State.inspect { case (s, _) => s.values.toList }

}
