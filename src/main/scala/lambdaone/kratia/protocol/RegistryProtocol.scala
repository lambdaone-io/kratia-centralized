package lambdaone.kratia.protocol

import lambdaone.kratia.registry.{Community, Member}

object RegistryProtocol {

  case class RegisterRequest[A, D](community: Community[A, D], data: D)

  case class RegisterResponse[A, D](member: Member[A, D])

}
