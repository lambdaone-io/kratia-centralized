package lambdaone.toolbox.mem

import lambdaone.toolbox.{CRUDGen, UniqueGen}
import cats.effect.IO
import cats.effect.concurrent.Ref

case class CRUDGenInMem[I, A](gen: UniqueGen[IO, I], withRef: Ref[IO, Map[I, A]]) extends CRUDGen[IO, I, A] with CRUDStoreInMem[I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): IO[I] =
    for {
      newId <- gen.gen
      _ <- withRef.update(_ + (newId -> a))
    } yield newId

}

