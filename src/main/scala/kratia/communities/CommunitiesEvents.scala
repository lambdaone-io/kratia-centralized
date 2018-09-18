package kratia.communities

import kratia.utils.Address

sealed trait CommunitiesEvents

object CommunitiesEvents {

  case object CommunitiesBooted extends CommunitiesEvents

  case class CommunityCreated(address: Address, name: String, domain: String) extends CommunitiesEvents
}

