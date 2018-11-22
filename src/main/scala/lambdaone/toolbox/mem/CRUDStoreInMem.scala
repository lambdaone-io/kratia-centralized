package lambdaone.toolbox.mem

import cats.effect.IO
import cats.effect.concurrent.Ref
import lambdaone.toolbox.CRUDStore

object CRUDStoreInMem {

  def apply[I, A](ref: Ref[IO, Map[I, A]]): CRUDStoreInMem[I, A] =
    new CRUDStoreInMem[I, A] {
      override protected val withRef: Ref[IO, Map[I, A]] = ref
    }

}

trait CRUDStoreInMem[I, A] extends CRUDStore[IO, I, A] {

  protected def withRef: Ref[IO, Map[I, A]]

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): IO[Option[A]] =
    withRef.get.map(_.get(id))

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): IO[Option[A]] =
    withRef.modify { state =>
      state.get(id) match {
        case Some(a) => (state + (id -> f(a))) -> Some(f(a))
        case None => state -> None
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): IO[Option[A]] =
    withRef.modify { state =>
      state.get(id) match {
        case Some(a) => (state - id) -> Some(a)
        case None => state -> None
      }
    }

  /** Returns all data within the store */
  override def all: IO[Map[I, A]] =
    withRef.get
}
