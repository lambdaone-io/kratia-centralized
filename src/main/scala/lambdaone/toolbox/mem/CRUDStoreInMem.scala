package lambdaone.toolbox.mem

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import lambdaone.toolbox.{CRUDStore, |->}
import CRUDStoreInMem._
import cats.data.Kleisli
import cats.{Id, ~>}

object CRUDStoreInMem {

  type Imports[A] = Ref[IO, UUID |-> A]

  type InMem[A, T] = Kleisli[IO, Imports[A], T]

  def apply[A]: CRUDStoreInMem[A] = new CRUDStoreInMem()

  def idGenerator: IO[UUID] = IO(UUID.randomUUID())

  def getRef[A]: InMem[A, Imports[A]] =
    Kleisli.ask

  def lift[A, T](io: IO[T]): InMem[A, T] =
    Kleisli.liftF(io)

  def withRef[A, T](f: Imports[A] => IO[T]): InMem[A, T] =
    Kleisli.ask[IO, Imports[A]].flatMapF(f)

  def buildStore[A]: IO[Imports[A]]=
    Ref.of[IO, UUID |-> A](Map.empty)

  def Interpreter[A]: InMem[A, ?] ~> Id =
    new (InMem[A, ?] ~> Id) {
      override def apply[T](fa: InMem[A, T]): Id[T] =
        buildStore[A].flatMap(fa.run).unsafeRunSync()
    }
}

class CRUDStoreInMem[A] extends CRUDStore[InMem[A, ?], UUID, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): InMem[A, UUID] =
    for {
      ref <- getRef
      newId <- lift(idGenerator)
      _ <- lift(ref.update(_ + (newId -> a)))
    } yield newId

  /** Uses a reference to try to look for the data in the store */
  override def get(id: UUID): InMem[A, Option[A]] =
    withRef(_.get.map(_.get(id)))

  /** Uses a reference to try to look for the data in the store, if found, applies `f` and stores the result,
    * returns the new version if the data if successful */
  override def update(id: UUID)(f: A => A): InMem[A, Option[A]] =
    withRef(_.modify { state =>
      state.get(id) match {
        case Some(a) => (state + (id -> f(a))) -> Some(f(a))
        case None => state -> None
      }
    })

  /** Uses a reference to try to delete the data, returns it if successful */
  override def delete(id: UUID): InMem[A, Option[A]] =
    withRef(_.modify { state =>
      state.get(id) match {
        case Some(a) => (state - id) -> Some(a)
        case None => state -> None
      }
    })

  /** Returns all data within the store */
  override def all: InMem[A, List[A]] =
    withRef(_.get.map(_.values.toList))
}

