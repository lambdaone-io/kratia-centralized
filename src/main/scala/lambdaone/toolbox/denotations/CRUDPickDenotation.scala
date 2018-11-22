package lambdaone.toolbox.denotations

import cats.data.State
import lambdaone.toolbox.CRUDPick
import lambdaone.toolbox.denotations.CRUDStoreDenotation.Denotation

case class CRUDPickDenotation[I, A]() extends CRUDPick[Denotation[I, A, ?], I, A] with CRUDStoreDenotation[I, A] {

  /** Store `a` with chosen id `id`, returns true if success, false if there was already an element with such id */
  override def create(a: A, id: I): Denotation[I, A, I] = {
    State { s =>
      if (s.contains(id)) s -> id
      else s + (id -> a) -> id
    }
  }

}

