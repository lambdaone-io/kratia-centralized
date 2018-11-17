package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Eq, Id, Monad, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.Ballot
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait CollectorTests[F[_], I, P] extends Laws {

  def laws: CollectorLaws[F, I, P]

  def collector(implicit arbA: Arbitrary[Ballot[P]],
                eqA: Eq[P],
                monad: Monad[F],
                ordering: Ordering[P]): RuleSet =
    new DefaultRuleSet(
      "collector",
      None,
      "new Ballot boxes have empty validateVote" -> forAll(laws.newBallotBoxes _)
    )
}

object CollectorTests {

  def apply[F[_], I, P](implicit store: Collector[F, I, P], run: F ~> Id): CollectorTests[F, I, P] =
    new CollectorTests[F, I, P] { def laws: CollectorLaws[F, I, P] = CollectorLaws[F, I, P] }
}


