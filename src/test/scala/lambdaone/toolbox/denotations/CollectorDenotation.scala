package lambdaone.toolbox.denotations

import cats.data.State
import cats.{Id, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, BallotBox, InfluenceAllocation}
import lambdaone.toolbox.denotations.CollectorDenotation.Denotation
import lambdaone.toolbox.denotations.UniqueGenDenotation.{Generator, LastGenerated}


object CollectorDenotation {

  type Denotation[I, P] = State[(UniqueGenDenotation.Denotation[I, I], CRUDStoreDenotation.Denotation[I, BallotBox[I, P]]), BallotBox[I, P]]

  def Interpreter[I, A](implicit numeric: Numeric[I]): Denotation[I, ?] ~> Id =
    new ( Denotation[I, ?] ~> Id ) {
      override def apply[T](fa: Denotation[I, T]): Id[T] =
        fa.runA((new UniqueGenDenotation[I](), new CRUDStoreDenotation[I, BallotBox[I, T]])()).value
    }

  def apply[I, A]: CollectorDenotation[I, A] = new CollectorDenotation()
}

class CollectorDenotation[Address, P] extends Collector[Denotation[Address, ?], Address, P] {

  import CollectorDenotation._

  override def create(ballot: Ballot[P], nickname: String): Denotation[Address, BallotBox[Address, P]] =
  State {
    case (genDen, storeDen ) => {
      ???
    }

  }

  override def vote(ballotBox: BallotBox[Address, P], vote: Collector.Vote[Address, P]): Denotation[Address, Collector.ProofOfVote[Address]] = ???

  override def validateVote(proofOfVote: Collector.ProofOfVote[Address]): Denotation[Address, Boolean] = ???

  override def inspect(ballotBox: BallotBox[Address, P]): Denotation[Address, InfluenceAllocation[P]] = ???
}
