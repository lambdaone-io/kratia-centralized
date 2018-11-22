package lambdaone.toolbox.mem

import lambdaone.toolbox.CRUDPick
import lambdaone.toolbox.mem.CRUDStoreInMem.InMem

case class CRUDPickInMem[I, A]() extends CRUDPick[InMem[I, A, ?], I, A] with CRUDStoreInMem[I, A] {

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def create(a: A, id: I): InMem[I, A, I] =
    withRef(_.modify { state =>
      if (state.contains(id)) state -> id
      else (state + (id -> a)) -> id
    })

}


