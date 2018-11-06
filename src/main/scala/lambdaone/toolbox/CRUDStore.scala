package lambdaone.toolbox

import cats.data.State
import cats.implicits._
import cats.{Functor, Id, Monad, MonadError, ~>}
import lambdaone.toolbox.UniqueGen.{UniqueGenReference, UniqueInt}

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

  type CRUDStoreReference[S, A] = State[(Map[Int, S], UniqueInt), A]

  implicit def referenceInterpreter[S]: Interpreter[CRUDStore[?, Int, S], CRUDStoreReference[S, ?], Id] =
    new Interpreter[CRUDStore[?, Int, S], CRUDStoreReference[S, ?], Id] {

      implicit val idGenInterpreter: Interpreter[UniqueGen[?, UniqueInt], UniqueGenReference, CRUDStoreReference[S, ?]] =
        new Interpreter[UniqueGen[?, UniqueInt], UniqueGenReference, CRUDStoreReference[S, ?]] {

          override def algebra: UniqueGen[UniqueGenReference, UniqueInt] =
            UniqueGen.uniqueGenFromStateMonad[UniqueGenReference]

          override def interpret: UniqueGenReference ~> CRUDStoreReference[S, ?] =
            new (UniqueGenReference ~> CRUDStoreReference[S, ?]) {
              override def apply[A](fa: UniqueGenReference[A]): CRUDStoreReference[S, A] =
                fa.contramap[(Map[Int, S], UniqueInt)](_._2)
            }

        }

      override def algebra: CRUDStore[CRUDStoreReference[S, ?], Int, S] =
        CRUDStore[CRUDStoreReference[S, ?], Int, S]

      override def interpret: CRUDStoreReference[S, ?] ~> Id =
        new (CRUDStoreReference[S, ?] ~> Id) {
          override def apply[A](fa: CRUDStoreReference[S, A]): Id[A] = fa.run((Map.empty, UniqueInt(0))).value._2
        }
    }

  def apply[F[_], I, A](implicit store: CRUDStore[F, I, A]): CRUDStore[F, I, A] = store

  implicit def storeFromStateMonad[F[_]: Monad, UniqueGenF[_]: Monad, I, A](
    implicit
    idGen: Interpreter[UniqueGen[?, I], UniqueGenF, F],
    state: StateMonad[F, Map[I, A]]
  ): CRUDStore[F, I, A] =
    new CRUDFromStateMonad[F, UniqueGenF, I, A](idGen, state)
}

class CRUDFromStateMonad[F[_]: Monad, UniqueGenF[_]: Monad, I, A] private[toolbox](idGen: Interpreter[UniqueGen[?, I], UniqueGenF, F], state: StateMonad[F, Map[I, A]]) extends CRUDStore[F, I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): F[I] =
    for {
      newId <- idGen.run(_.gen)
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