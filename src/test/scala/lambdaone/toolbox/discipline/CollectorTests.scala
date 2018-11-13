package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.collector.Collector
import lambdaone.toolbox.CRUDStore
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait CollectorTests[F[_], I, A] extends Laws {

  def laws: CollectorLaws[F, I, A]

  def crud(implicit arbA: Arbitrary[A], eqA: Eq[A], monad: Monad[F], monoid: Monoid[A], ordering: Ordering[A]): RuleSet =
    new DefaultRuleSet(
      "crud",
      None,
      "fetching" -> forAll(laws.creating _)
    )
}

object CollectorTests {

  def apply[F[_], I, A](implicit store: Collector[F, I, A], run: F ~> Id): CollectorTests[F, I, A] =
    new CRUDStoreTests[F, I, A] { def laws: CRUDStoreLaws[F, I, A] = CollectorLaws[F, I, A] }
}


