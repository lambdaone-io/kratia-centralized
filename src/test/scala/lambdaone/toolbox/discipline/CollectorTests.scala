package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.BinaryProposal
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait CollectorTests[F[_], I] extends Laws {

  def laws: CollectorLaws[F, I]

  def collector(implicit arbA: Arbitrary[BinaryProposal], eqA: Eq[BinaryProposal], monad: Monad[F], monoid: Monoid[BinaryProposal], ordering: Ordering[BinaryProposal]): RuleSet =
    new DefaultRuleSet(
      "collector",
      None,
      "new Ballot boxes have empty validateVote" -> forAll(laws.newBallotBoxes _)
    )
}

object CollectorTests {

  def apply[F[_], I, A](implicit store: Collector[F, I, A], run: F ~> Id): CollectorTests[F, I] =
    new CollectorTests[F, I] { def laws: CollectorLaws[F, I] = CollectorLaws[F, I] }
}


