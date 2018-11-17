package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Id, Monad, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, BinaryProposal, InfluenceAllocation}
import lambdaone.helpers.{IsEq, _}

trait CollectorLaws[F[_], I, P] {

  def coll: Collector[F, I, P]

  def interpret: F ~> Id

  def newBallotBoxes(xs: List[Ballot[P]])(implicit F: Monad[F], ordering: Ordering[P]): IsEq[Collector.InfluenceAllocation[P]] = {
    val program =
      xs.traverse(a => coll.create(a, a.toString)) >>= ( bbs => bbs.traverse(bb => coll.inspect(bb)) )
    val calculated = interpret(program).fold(Map())(_ ++ _)
    IsEq(calculated, Map())
  }
}


object CollectorLaws {

  def apply[F[_], I, P](implicit collector: Collector[F, I, P], run: F ~> Id): CollectorLaws[F, I, P] =
    new CollectorLaws[F, I, P] {

      override def coll: Collector[F, I, P] = collector

      override def interpret: F ~> Id = run
    }
}


