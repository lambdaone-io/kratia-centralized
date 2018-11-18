package lambdaone.toolbox.denotations

import cats.arrow.FunctionK
import cats.data.{Kleisli, State}
import cats.{Id, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, BallotBox, InfluenceAllocation}
import lambdaone.toolbox.denotations.CRUDStoreDenotation.Denotation
import lambdaone.toolbox.denotations.CollectorDenotation.Denotation


object CollectorDenotation {

  type Generator[I] = I => I

  type GeneratorState[I] = I

  type CollectorState[I, A] = (Map[I, A], GeneratorState[I])

  type Denotation[I, A, T] = Kleisli[State[CollectorState[I, A], ?], Generator[I], T]

  def run[I, A](initial: (Map[I, A], I), generator: I => I): Denotation[I, A, ?] ~> Id =
    new FunctionK[Denotation[I, A, ?], Id] {
      def apply[T](fa: Denotation[I, A, T]): Id[T] =
        fa.run(generator).run(initial).value._2
    }
}

case class CollectorDenotation[Address, P] extends Collector[Denotation[Address, ?], Address, P] {

  import CollectorDenotation._

  private def generateId: Denotation[I, A, I] =
    gen.gen.mapF {
      _.transformS(
        _._2,
        (r, sb) => (r._1, sb)
      )
    }

  override def create(ballot: Ballot[P], nickname: String): Denotation[Address, BallotBox[Address, P]] =
  State {
    case (genDen, storeDen ) => {
      val g: UniqueGenDenotation.Denotation[Address, Address] = genDen.gen
      ???
    }

  }

  override def vote(ballotBox: BallotBox[Address, P], vote: Collector.Vote[Address, P]): Denotation[Address, Collector.ProofOfVote[Address]] = ???

  override def validateVote(proofOfVote: Collector.ProofOfVote[Address]): Denotation[Address, Boolean] = ???

  override def inspect(ballotBox: BallotBox[Address, P]): Denotation[Address, InfluenceAllocation[P]] = ???
}
