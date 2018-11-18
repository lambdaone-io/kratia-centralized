package lambdaone.toolbox.discipline

import cats.implicits._
import cats.{Id, Monad, Monoid, ~>}
import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, InfluenceAllocation}
import lambdaone.helpers.{IsEq, _}

trait CollectorLaws[F[_], D[_], I, P] {

  implicit def implementation: Collector[F, I, P]

  implicit def denotation: Collector[D, I, P]

  implicit val F: Monad[F]

  implicit val D: Monad[D]

  def implement: F ~> Id

  def denote: D ~> Id


  def newBallotBoxes(xs: List[Ballot[P]])(implicit F: Monad[F], ordering: Ordering[P]): IsEq[Collector.InfluenceAllocation[P]] = {
    def program[G[_]](implicit G: Monad[G], cat: Collector[G, I, P]): G[List[InfluenceAllocation[P]]] =
      xs.traverse(a => cat.create(a, a.toString)) >>= ( bbs => bbs.traverse(bb => cat.inspect(bb)) )

    implement(program[F]).sorted <-> denote(program[D]).sorted
  }
}

object CollectorLaws {

  def apply[F[_], D[_], I, A](implicit
                              implementation0: Collector[F, I, A],
                              denotation0: Collector[D, I, A],
                              F0: Monad[F],
                              D0: Monad[D],
                              ordering0: Ordering[A],
                              monoid0: Monoid[A],
                              interpret0: F ~> Id,
                              denote0: D ~> Id
                             ): CollectorLaws[F, D, I, A] =
    new CollectorLaws[F, D, I, A] {
      def implementation: Collector[F, I, A] = implementation0

      def denotation: Collector[D, I, A] = denotation0

      implicit val F: Monad[F] = F0
      implicit val D: Monad[D] = D0
      implicit val ordering: Ordering[A] = ordering0

      def implement: F ~> Id = interpret0

      def denote: D ~> Id = denote0
    }
}


