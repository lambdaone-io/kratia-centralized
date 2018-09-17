package kratia.communities

import kratia.communities.communities_decision._
import kratia.kratia_core_model.Address

object communities_events {

  sealed trait CommunityEvents

  object CommunityEvents {

    case class CommunityCreated(name: String) extends CommunityEvents

    case class Voted(vote: Vote, decision: Decision) extends CommunityEvents

    case class Resolved(ballot: Address, decision: Decision, vote: Vote) extends CommunityEvents

    case class MemberAdded(member: Address, nickname: String) extends CommunityEvents

    case class DecisionSystemChanged(community: Address, decisionType: DecisionType, allocation: InfluenceDistributionType, resolution: DecisionResolutionType) extends CommunityEvents
  }

}
