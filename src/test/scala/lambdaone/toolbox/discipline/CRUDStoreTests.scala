package lambdaone.toolbox.discipline

import cats.implicits._
import org.scalacheck.Prop.forAll
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.toolbox.CRUDStore
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait CRUDStoreTests[F[_], I, A] extends Laws {

  def laws: CRUDStoreLaws[F, I, A]

  def crud(implicit arbA: Arbitrary[A], eqA: Eq[A], monad: Monad[F], monoid: Monoid[A], ordering: Ordering[A]): RuleSet =
    new DefaultRuleSet(
      "crud",
      None,
      "fetching" -> forAll(laws.fetching _),
      "creation" -> forAll(laws.creation _),
      "deletion" -> forAll(laws.deletion _),
      "updates" -> forAll(laws.updates _)
    )
}

object CRUDStoreTests {

  def apply[F[_], I, A](implicit store: CRUDStore[F, I, A], run: F ~> Id): CRUDStoreTests[F, I, A] =
    new CRUDStoreTests[F, I, A] { def laws: CRUDStoreLaws[F, I, A] = CRUDStoreLaws[F, I, A] }
}
