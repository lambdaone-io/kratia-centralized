package lambdaone.kratia.registry

import cats.{Id, ~>}
import lambdaone.toolbox.denotations.CRUDStoreDenotation

object RegistryDenotation {

  type Denotation[A, D, T] = CRUDStoreDenotation.Denotation[(A, A), D, T]

  def RegistryDenotation[A, D]: Registry[Denotation[A, D, ?], A, D] =
    RegistryCRUD(CRUDStoreDenotation[(A, A), D]())

  def run[A, D](initial: (Map[(A, A), D], (A, A)), generator: Tuple2[A, A] => (A, A)): Denotation[A, D, ?] ~> Id =
    CRUDStoreDenotation.run[(A, A), D](initial, generator)
}
