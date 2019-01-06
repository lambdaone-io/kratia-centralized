package lambdaone.kratia.resolution

import java.util.UUID

import lambdaone.kratia.collector.Proposal.BinaryProposal
import lambdaone.kratia.collector.{DecisionData, Timestamp}

case class Resolution(address: UUID, closedOn: Timestamp, data: DecisionData, maxInfluence: Double, resolution: Set[BinaryProposal])
