package lambdaone.toolbox.denotations

import cats.data.State
import cats.{Id, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.InfluenceAllocation
import lambdaone.toolbox.denotations.CollectorDenotation.Denotation


object CollectorDenotation {

  type LastIds[Address] = List[Address]

  type AddressGenerator[Address] = List[Address] => Address

  type IdGenerator[I] = List[I] => I

  type LastStorage[I, A] = Map[I, A]

  type Denotation[I, A, T] = State[(LastStorage[I, A]), T]

  def numericGenerator[I](implicit numeric: Numeric[I]): AddressGenerator[I] = {
    case Nil => numeric.zero
    case x :: _ => numeric.plus(x, numeric.one)
  }

  def Interpreter[I, A](implicit numeric: Numeric[I]): Denotation[I, A, ?] ~> Id =
    new ( Denotation[I, A, ?] ~> Id ) {
      override def apply[T](fa: Denotation[I, A, T]): Id[T] =
        fa.runA((Map.empty)).value
    }

  def apply[I, A]: CollectorDenotation[I, A] = new CollectorDenotation()
}

class CollectorDenotation[Address, P] extends Collector[Denotation[Address, P, ?], Address, P] {

  import CollectorDenotation._

  override def create(ballot: Collector.Ballot[P], nickname: String): Denotation[Address, P, Collector.BallotBox[Address, P]] = ???

  override def vote(ballotBox: Collector.BallotBox[Address, P], vote: Collector.Vote[Address, P]): Denotation[Address, P, Collector.ProofOfVote[Address]] = ???

  override def validateVote(proofOfVote: Collector.ProofOfVote[Address]): Denotation[Address, P, Boolean] = ???

  override def inspect(ballotBox: Collector.BallotBox[Address, P]): Denotation[Address, P, InfluenceAllocation[P]] = ???
}
