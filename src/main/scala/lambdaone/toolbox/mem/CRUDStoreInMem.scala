package lambdaone.toolbox.mem

import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.{Id, ~>}
import lambdaone.toolbox.CRUDStore
import lambdaone.toolbox.mem.CRUDStoreInMem.{Imports, InMem}

object CRUDStoreInMem {

  def apply[I, A]: CRUDStoreInMem[I, A] =
    new CRUDStoreInMem[I, A] {}

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

trait CRUDStoreInMem[I, A] extends CRUDStore[InMem[I, A, ?], I, A] {

  protected def withRef[T](f: Ref[IO, Map[I, A]] => IO[T]): InMem[I, A, T] =
    Kleisli.ask[IO, Imports[I, A]].flatMapF(f)

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
