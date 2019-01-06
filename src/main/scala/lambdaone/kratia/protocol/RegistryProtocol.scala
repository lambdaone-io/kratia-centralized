package lambdaone.kratia.protocol

import lambdaone.kratia.registry.{Community, Member}

object RegistryProtocol {

  case class RegisterRequest(community: Community, data: MemberData)

  case class RegisterResponse(member: Member)

}
