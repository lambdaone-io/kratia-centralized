package lambdaone.toolbox.mem

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import lambdaone.toolbox.{CRUDStore, |->}
import CRUDStoreInMem._
import cats.data.Kleisli
import cats.{Id, ~>}

object CRUDStoreInMem {

  type Generator[I] = IO[I]

  type Imports[I, A] = (Generator[I], Ref[IO, I |-> A])

  type InMem[I, A, T] = Kleisli[IO, Imports[I, A], T]

  def apply[I, A]: CRUDStoreInMem[I, A] = new CRUDStoreInMem[I, A]

  def uuidGenerator: IO[UUID] = IO(UUID.randomUUID())

  def tupledUUIDGenerator: IO[(UUID, UUID)] = (uuidGenerator, uuidGenerator).mapN(_ -> _)

  def withRef[I, A, T](f: Ref[IO, I |-> A] => IO[T]): InMem[I, A, T] =
    Kleisli.ask[IO, Imports[I, A]].map(_._2).flatMapF(f)

  def generateId[I, A]: InMem[I, A, I] =
    Kleisli.ask[IO, Imports[I, A]].map(_._1).flatMapF(identity)

  def buildStore[I, A]: IO[Ref[IO, I |-> A]] =
    Ref.of[IO, I |-> A](Map.empty)

  def Interpreter[I, A](generator: Generator[I]): InMem[I, A, ?] ~> Id =
    new (InMem[I, A, ?] ~> Id) {
      override def apply[T](fa: InMem[I, A, T]): Id[T] =
        buildStore[I, A].flatMap(store => fa.run(generator -> store)).unsafeRunSync()
    }
}

class CRUDStoreInMem[I, A] extends CRUDStore[InMem[I, A, ?], I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): InMem[I, A, I] =
    for {
      newId <- generateId[I, A]
      _ <- withRef[I, A, Unit](_.update(_ + (newId -> a)))
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
  override def all: InMem[I, A, List[A]] =
    withRef(_.get.map(_.values.toList))
}

