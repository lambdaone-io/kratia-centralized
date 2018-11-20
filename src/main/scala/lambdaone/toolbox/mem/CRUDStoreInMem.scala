package lambdaone.toolbox.mem

import cats.effect.concurrent.Ref
import lambdaone.toolbox.{CRUDStore, UniqueGen}
import CRUDStoreInMem._
import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.effect.IO
import cats.{Id, ~>}

object CRUDStoreInMem {

  type Imports[I, A] = Ref[IO, Map[I, A]]

  type InMem[I, A, T] = Kleisli[IO, Imports[I, A], T]

  def runEffectWith[I, A](imports: Imports[I, A]): InMem[I, A, ?] ~> IO =
    new FunctionK[InMem[I, A, ?], IO] {
      def apply[T](fa: InMem[I, A, T]): IO[T] =
        fa.run(imports)
    }

  def runUnsafeSync[I, A]: InMem[I, A, ?] ~> Id =
    new FunctionK[InMem[I, A, ?], Id] {
      def apply[T](fa: InMem[I, A, T]): Id[T] =
        fa.run(Ref.of[IO, Map[I, A]](Map.empty).unsafeRunSync()).unsafeRunSync()
    }

}

case class CRUDStoreInMem[I, A](gen: UniqueGen[IO, I]) extends CRUDStore[InMem[I, A, ?], I, A] {

  private def withRef[T](f: Ref[IO, Map[I, A]] => IO[T]): InMem[I, A, T] =
    Kleisli.ask[IO, Imports[I, A]].flatMapF(f)

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): InMem[I, A, I] =
    for {
      newId <- Kleisli.liftF(gen.gen)
      _ <- withRef(_.update(_ + (newId -> a)))
    } yield newId

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  def createPick(a: A, id: I): InMem[I, A, Boolean] =
    withRef(_.modify { state =>
      if (state.contains(id)) state -> false
      else (state + (id -> a)) -> true
    })

  /** Uses a reference to try to look for the data in the store */
  override def get(id: I): InMem[I, A, Option[A]] =
    withRef(_.get.map(_.get(id)))

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: I)(f: A => A): InMem[I, A, Option[A]] =
    withRef(_.modify { state =>
      state.get(id) match {
        case Some(a) => (state + (id -> f(a))) -> Some(f(a))
        case None => state -> None
      }
    })

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: I): InMem[I, A, Option[A]] =
    withRef(_.modify { state =>
      state.get(id) match {
        case Some(a) => (state - id) -> Some(a)
        case None => state -> None
      }
    })

  /** Returns all data within the store */
  override def all: InMem[I, A, Map[I, A]] =
    withRef(_.get)

}

