package lambdaone.kratia.protocol

import java.util.UUID

import lambdaone.kratia.collector._

object CollectorProtocol {

  case class CreateBallotBoxRequest(validBallot: Ballot, data: DecisionData, closesOn: Timestamp)

  case class CreateBallotBoxResponse(data: BallotMetadata)

  case class SetVoteRequest(ballotBox: BallotBox, vote: InfluenceAllocation)

  case class SetVoteResponse(proof: UUID)

}
