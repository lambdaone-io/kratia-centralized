package lambdaone.toolbox.denotations

import cats.data.{Kleisli, State}
import lambdaone.toolbox.CRUDStore
import CRUDStoreDenotation._
import cats.arrow.FunctionK
import cats.{Id, ~>}

object CRUDStoreDenotation {

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

case class CRUDStoreDenotation[I, A]() extends CRUDStore[Denotation[I, A, ?], I, A] {

  private val gen: UniqueGenDenotation[I] = UniqueGenDenotation()

  private def modify(f: Map[I, A] => Map[I, A]): Denotation[I, A, Unit] =
    state(s => (f(s), ()))

  private def inspect[T](f: Map[I, A] => T): Denotation[I, A, T] =
    state(s => (s, f(s)))

  private def state[T](f: Map[I, A] => (Map[I, A], T)): Denotation[I, A, T] =
    Kleisli.liftF { State { case (s, i) =>
      val (ns, t) = f(s)
      (ns, i) -> t
    } }

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

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def createPick(a: A, id: I): Denotation[I, A, Boolean] = {
    state { s =>
      if (s.contains(id)) s -> false
      else s + (id -> a) -> true
    }
  }

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): Denotation[I, A, Option[A]] =
    inspect { s => s.get(id) }

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): Denotation[I, A, Option[A]] =
    state { s =>
      s.get(id) match {
        case None => s -> None
        case Some(a) => (s + (id -> a)) -> Some(f(a))
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): Denotation[I, A, Option[A]] =
    state { s =>
      s.get(id) match {
        case None => s -> None
        case Some(a) => (s - id) -> Some(a)
      }
    }

  /** Returns all data within the store */
  override def all: Denotation[I, A, List[A]] =
    inspect { s => s.values.toList }

}
