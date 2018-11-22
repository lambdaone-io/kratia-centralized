package lambdaone.toolbox.mem

import lambdaone.toolbox.{CRUDGen, UniqueGen}
import cats.data.Kleisli
import cats.effect.IO
import lambdaone.toolbox.mem.CRUDStoreInMem.InMem

case class CRUDGenInMem[I, A](gen: UniqueGen[IO, I]) extends CRUDGen[InMem[I, A, ?], I, A] with CRUDStoreInMem[I, A] {

  /** Stores `a` and produces a new unique reference */
  override def create(a: A): InMem[I, A, I] =
    for {
      newId <- Kleisli.liftF(gen.gen)
      _ <- withRef(_.update(_ + (newId -> a)))
    } yield newId

}

