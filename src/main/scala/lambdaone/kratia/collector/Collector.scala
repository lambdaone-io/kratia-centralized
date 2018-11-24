package lambdaone.kratia.collector

trait Collector[F[_], Address, P] {

  def create(ballot: Ballot[P], closesOn: Timestamp): F[BallotBox[Address, P]]

  def vote(ballotBox: BallotBox[Address, P], vote: Vote[Address, P]): F[ProofOfVote[Address]]

  def validateVote(ballotBox: BallotBox[Address, P], proofOfVote: ProofOfVote[Address]): F[Boolean]

  def inspect(ballotBox: BallotBox[Address, P]): F[InfluenceAllocation[P]]

}
