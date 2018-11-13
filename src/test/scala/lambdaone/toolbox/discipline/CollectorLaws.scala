package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Id, Monad, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, BinaryProposal, InfluenceAllocation}
import lambdaone.helpers.{IsEq, _}

// P is fixed to BinaryProposal
trait CollectorLaws[F[_], I] {

  def coll: Collector[F, I, BinaryProposal]

  def interpret: F ~> Id

  def newBallotBoxes(xs: List[BinaryProposal])(implicit F: Monad[F], ordering: Ordering[BinaryProposal]): IsEq[Collector.InfluenceAllocation[BinaryProposal]] = {
    val program =
      xs.traverse(a => coll.create(Collector.binaryBallot, a.toString)) >>= ( bbs => bbs.traverse(bb => coll.inspect(bb)) )
    val calculated = interpret(program).fold(Map())(_ ++ _)
    IsEq(calculated, Map())
  }

}


object CollectorLaws {

  def apply[F[_], I](implicit collector: Collector[F, I, BinaryProposal], run: F ~> Id): CollectorLaws[F, I] =
    new CollectorLaws[F, I] {

      override def coll: Collector[F, I, BinaryProposal] = collector

      override def interpret: F ~> Id = run
    }
}


