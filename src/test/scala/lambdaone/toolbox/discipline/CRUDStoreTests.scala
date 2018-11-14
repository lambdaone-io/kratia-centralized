package lambdaone.toolbox.discipline

import cats.implicits._
import org.scalacheck.Prop.forAll
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.toolbox.CRUDStore
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait CRUDStoreTests[F[_], D[_], I, A] extends Laws {

  def laws: CRUDStoreLaws[F, D, I, A]

  def crud(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
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

  def apply[F[_]: Monad, D[_]: Monad, I, A: Monoid: Ordering](implicit
    implementation0: CRUDStore[F, I, A],
    denotation0: CRUDStore[D, I, A],
    interpret0: F ~> Id,
    denote0: D ~> Id
  ): CRUDStoreTests[F, D, I, A] =
    new CRUDStoreTests[F, D, I, A] { def laws: CRUDStoreLaws[F, D, I, A] = CRUDStoreLaws[F, D, I, A] }
}
