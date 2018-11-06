package lambdaone

import cats.kernel.Eq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty

package object helpers {

  implicit final class IsEqArrow[A](val lhs: A) extends AnyVal {
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }

  implicit def catsLawsIsEqToProp[A](isEq: IsEq[A])(implicit ev: Eq[A], pp: A => Pretty): Prop =
    isEq match {
      case IsEq(x, y) =>
        if (ev.eqv(x, y)) Prop.proved
        else
          Prop.falsified :| {
            val exp = Pretty.pretty[A](y, Pretty.Params(0))
            val act = Pretty.pretty[A](x, Pretty.Params(0))
            s"Expected: $exp\n" + s"Received: $act"
          }
    }
}
