package lambdaone.kratia.protocol

import lambdaone.kratia.collector._

object CollectorProtocol {

  case class CreateBallotBoxRequest[P, D](validBallot: Ballot[P], data: D, closesOn: Timestamp)

  case class CreateBallotBoxResponse[A, P, D](data: BallotMetadata[A, P, D])

  case class SetVoteRequest[A, P](ballotBox: BallotBox[A, P], vote: InfluenceAllocation[P])

  case class SetVoteResponse[A](proof: A)

}
