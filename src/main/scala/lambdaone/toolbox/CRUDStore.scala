package lambdaone.toolbox

import cats.implicits._
import cats.{Functor, Monad, MonadError, ~>}

/** CRUD operations based on a type for identifying data (like UUID or a hash) to store some data A
  *
  * @tparam F the wrapping effect
  * @tparam I identification or reference to an instance of A, used to fetch the data and should be unique within the store
  * @tparam A data to be stored and retrieved
  */
trait CRUDStore[F[_], I, A] {

  /** Stores `a` and produces a new unique reference */
  def create(a: A): F[I]

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
  def all: F[Iterable[A]]

  /** Returns true if data with such reference exists in the store */
  def exists(id: I): F[Boolean]

  /** Selection of a subset of the data */
  def filter(f: A => Boolean): F[Iterable[A]]

  /** Delete and ignore result */
  def delete_(id: I)(implicit F: Functor[F]): F[Unit] =
    delete(id).void
}

object CRUDStore {

  //implicit def fromG[F[_], G[_], I, A](implicit map: G ~> F, impl: Store[G, I, A]): Store[F, I, A]

  implicit def storeReference[F[_]: Monad, G[_]: Monad, I, A](implicit idGen: UniqueGen[G, I], runGen: G ~> F, state: StateMonad[F, Map[I, A]]): CRUDStore[F, I, A] =
    new CRUDStoreReference[F, G, I, A](idGen, runGen, state)
}

class CRUDStoreReference[F[_]: Monad, G[_]: Monad, I, A] private[toolbox](idGen: UniqueGen[G, I], runGen: G ~> F, state: StateMonad[F, Map[I, A]]) extends CRUDStore[F, I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): F[I] =
    for {
      newId <- runGen(idGen.gen)
      _ <- state.modify(_ + (newId -> a))
    } yield newId

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): F[Option[A]] =
    state.inspect(_.get(id))

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): F[Option[A]] =
    state.run { s =>
      s.get(id) match {
        case None => (s, None)
        case Some(a) => (s + (id -> a), Some(a))
      }
    }

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): F[Option[A]] =
    state.run { s =>
      s.get(id) match {
        case None => (s, None)
        case Some(a) => (s - id, Some(a))
      }
    }

  /** Returns all data within the store */
  override def all: F[Iterable[A]] =
    state.map(state.get)(_.values)

  /** Returns true if data with such reference exists in the store */
  override def exists(id: I): F[Boolean] =
    state.inspect(_.contains(id))

  /** Selection of a subset of the data */
  override def filter(f: A => Boolean): F[Iterable[A]] =
    state.map(all)(_.filter(f))
}