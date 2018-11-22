package lambdaone.kratia.registry

import cats.{Id, ~>}
import lambdaone.toolbox.denotations.{CRUDPickDenotation, CRUDStoreDenotation}

object RegistryDenotation {

  type Denotation[A, D, T] = CRUDStoreDenotation.Denotation[(A, A), D, T]

  def RegistryDenotation[A, D]: Registry[Denotation[A, D, ?], A, D] =
    RegistryCRUD(CRUDPickDenotation[(A, A), D]())

  def run[A, D](initial: Map[(A, A), D]): Denotation[A, D, ?] ~> Id =
    CRUDStoreDenotation.run[(A, A), D](initial)
}
