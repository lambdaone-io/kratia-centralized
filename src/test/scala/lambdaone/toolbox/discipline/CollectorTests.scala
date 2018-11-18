package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.Ballot
import lambdaone.toolbox.CRUDStore
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait CollectorTests[F[_], D[_], I, A] extends Laws {

  def laws: CollectorLaws[F, D, I, A]

  def implement: F ~> Id

  def denote: D ~> Id


  def collector(implicit arbA: Arbitrary[A], arbI: Arbitrary[I], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "crud",
      None,
      "fetching" -> forAll(laws.newBallotBoxes _)
    )
}

object CollectorTests {

  def apply[F[_]: Monad, D[_]: Monad, I, A: Monoid: Ordering](
                                                               implementation: CRUDStore[F, I, A],
                                                               denotation: CRUDStore[D, I, A],
                                                               implement: F ~> Id,
                                                               denote: D ~> Id
                                                             ): CRUDStoreTests[F, D, I, A] =
    new CRUDStoreTests[F, D, I, A] { def laws: CRUDStoreLaws[F, D, I, A] = CRUDStoreLaws[F, D, I, A](
      implementation,
      denotation,
      Monad[F],
      Monad[D],
      implicitly[Ordering[A]],
      Monoid[A],
      implement,
      denote
    ) }
}