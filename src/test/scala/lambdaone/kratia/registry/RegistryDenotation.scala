package lambdaone.kratia.registry

import lambdaone.toolbox.denotations.CRUDStoreDenotation

object RegistryDenotation {

  type Denotation[A, D, T] = CRUDStoreDenotation.Denotation[(A, A), D, T]

  def RegistryDenotation[A, D]: Registry[Denotation[A, D, ?], A, D] = new RegistryCRUD(CRUDStoreDenotation[(A, A), D]())

}
