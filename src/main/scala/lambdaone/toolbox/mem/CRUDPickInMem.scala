package lambdaone.toolbox.mem

import cats.effect.IO
import cats.effect.concurrent.Ref
import lambdaone.toolbox.CRUDPick

case class CRUDPickInMem[I, A](withRef: Ref[IO, Map[I, A]]) extends CRUDPick[IO, I, A] with CRUDStoreInMem[I, A] {

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def create(a: A, id: I): IO[I] =
    withRef.modify { state =>
      if (state.contains(id)) state -> id
      else (state + (id -> a)) -> id
    }

}


