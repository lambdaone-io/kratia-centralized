package lambdaone.toolbox.discipline

import cats.implicits._
import org.scalacheck.Prop.forAll
import cats.{Eq, Id, Monad, Monoid, ~>}
import lambdaone.toolbox.CRUDStore
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait CRUDStoreTests[F[_], D[_], I, A] extends Laws {

  def laws: CRUDStoreLaws[F, D, I, A]

  def crud(implicit arbA: Arbitrary[A], arbI: Arbitrary[I], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "crud",
      None,
      "fetching" -> forAll(laws.fetching _),
      "creation" -> forAll(laws.creation _),
      "deletion" -> forAll(laws.deletion _),
      "updates" -> forAll(laws.updates _),
      "creationPick" -> forAll(laws.creationPick _)
    )
}

object CRUDStoreTests {

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
