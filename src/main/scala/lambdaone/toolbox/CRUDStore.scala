package lambdaone.toolbox

import cats.implicits._
import cats.{Functor, MonadError}

/** CRUD operations based on a type for identifying data (like UUID or a hash) to store some data A
  *
  * @tparam F the wrapping effect
  * @tparam I identification or reference to an instance of A, used to fetch the data and should be unique within the store
  * @tparam A data to be stored and retrieved
  */
trait CRUDStore[F[_], I, A] {

  /** Uses a reference to try to look for the data in the store */
  def get(id: I): F[Option[A]]

  /** Uses a reference to try to look for the data in the store, fails with error `e` if not found */
  def read[E](id: I)(e: => E)(implicit F: MonadError[F, E]): F[A] =
    get(id).flatMap {
      case None => F.raiseError(e)
      case Some(a) => F.pure(a)
    }

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  def update(id: I)(f: A => A): F[Option[A]]

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * fails with error `e` if not found; returns the new version of the data
    */
  def force[E](id: I)(e: => E)(f: A => A)(implicit F: MonadError[F, E]): F[A] =
    get(id).flatMap {
      case None => F.raiseError(e)
      case Some(a) => F.pure(a)
    }

  /** Rewrite whole data into a complete new value */
  def set(id: I)(newValue: A): F[Option[A]] =
    update(id)(_ => newValue)

  /** Uses a reference to try to delete the data, returns it if successful */
  def delete(id: I): F[Option[A]]

  /** Returns all data within the store */
  def all: F[Map[I, A]]

  /** Returns true if data with such reference exists in the store */
  def exists(id: I)(implicit F: Functor[F]): F[Boolean] =
    get(id).map(_.isDefined)

  /** Selection of a subset of the data */
  def filter(f: A => Boolean)(implicit F: Functor[F]): F[Map[I, A]] =
    all.map(_.filter { case (_, a) => f(a) })

  /** Selection of a subset of the data based on an id property */
  def filterId(f: I => Boolean)(implicit F: Functor[F]): F[Map[I, A]] =
    all.map(_.filterKeys(f))

  /** Delete and ignore result */
  def delete_(id: I)(implicit F: Functor[F]): F[Unit] =
    delete(id).void
}

object CRUDStore {

  def apply[F[_], I, A](implicit store: CRUDStore[F, I, A]): CRUDStore[F, I, A] = store

}
