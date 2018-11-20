package lambdaone.kratia.collector

import lambdaone.kratia.collector.Collector.{Ballot, BallotBox, Vote}

sealed trait CollectorEvent[A, P]

object CollectorEvent {

  case class CreatedBallotBox[A, P](ref: A, ballot: Ballot[P]) extends CollectorEvent[A, P]

  case class Voted[A, P](proof: A, ballotBox: BallotBox[A, P], vote: Vote[A, P]) extends CollectorEvent[A, P]

}
