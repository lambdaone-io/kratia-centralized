package lambdaone.toolbox

import cats.{Eq, Id, Monad, Monoid}
import lambdaone.helpers._
import cats.kernel.Semigroup
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.FunSuite
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import cats.implicits._

trait CRUDStoreLaws[F[_], I, A] {

  implicit def crud: Interpreter[CRUDStore[?, I, A], F, Id]

  def creation(xs: List[A])(implicit F: Monad[F]): IsEq[List[A]] = {
    val program: F[List[A]] =
      xs.traverse(crud.algebra.create) *> crud.algebra.all.map(_.toList)
    crud.interpret(program) <-> xs
  }

  def creationDeletion(a: A)(implicit F: Monad[F]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] = for {
      id <- crud.algebra.create(a)
      optA0 <- crud.algebra.get(id)
      optA1 <- crud.algebra.delete(id)
      all <- crud.algebra.all
    } yield List(optA0, optA1) ++ all.toList.map(Option.apply)
    crud.interpret(program) <-> List(Some(a), Some(a))
  }

  def nonDestructiveUpdates(xs: List[A])(implicit F: Monad[F], monoid: Monoid[A]): IsEq[List[Option[A]]] = {
    val program: F[List[Option[A]]] = for {
      ids <- xs.traverse(crud.algebra.create)
      updates <- ids.traverse(crud.algebra.update(_)(_ |+| monoid.empty))
    } yield updates
    crud.interpret(program) <-> xs.map(Option.apply)
  }
}

trait CRUDStoreTests[F[_], I, A] extends Laws {

  def laws: CRUDStoreLaws[F, I, A]

  def crud(implicit arbA: Arbitrary[A], eqA: Eq[A], monad: Monad[F], monoid: Monoid[A]): RuleSet =
    new DefaultRuleSet(
      "crud store",
      None,
      "creation" -> forAll(laws.creation _),
      "creationDeletion" -> forAll(laws.creationDeletion _),
      "nonDestructiveUpdates" -> forAll(laws.nonDestructiveUpdates _)
    )
}

class CRUDStoreSpec extends FunSuite with Discipline {

  checkAll("CRUDStore[]")
  checkAll("Semigroup[Int]", SemigroupTests[Int].semigroup)
}

trait SemigroupLaws[A] {
  implicit def S: Semigroup[A]

  def semigroupAssociative(x: A, y: A, z: A): IsEq[A] =
    S.combine(S.combine(x, y), z) <-> S.combine(x, S.combine(y, z))

  def repeat1(a: A): IsEq[A] =
    S.combineN(a, 1) <-> a

  def repeat2(a: A): IsEq[A] =
    S.combineN(a, 2) <-> S.combine(a, a)

  def combineAllOption(xs: Vector[A]): IsEq[Option[A]] =
    S.combineAllOption(xs) <-> xs.reduceOption(S.combine)

}

object SemigroupLaws {
  def apply[A](implicit ev: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] { def S: Semigroup[A] = ev }
}

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def semigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "semigroup",
      None,
      "associative" -> forAll(laws.semigroupAssociative _),
      "repeat1" -> forAll(laws.repeat1 _),
      "repeat2" -> forAll(laws.repeat2 _),
      "combineAllOption" -> forAll(laws.combineAllOption _)
    )
}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}