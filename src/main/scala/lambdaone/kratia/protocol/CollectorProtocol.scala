package lambdaone.kratia.protocol

import lambdaone.kratia.collector.{Ballot, BallotMetadata, Timestamp}

object CollectorProtocol {

  case class CreateBallotBoxRequest[P, D](validBallot: Ballot[P], data: D, closesOn: Timestamp)

  case class CreateBallotBoxResponse[A, P, D](data: BallotMetadata[A, P, D])

}
