package lambdaone.kratia.resolution

import cats.Applicative

case class Majority()

object Majority {

  implicit def majority[F[_], P](implicit F: Applicative[F]): DecisionResolution[F, P, Majority] =
    (allocation, _, _) => {
      val resolution = allocation.value.foldLeft((Set.empty[P], 0.0)) {
        case ((winning, winningInf), (xp, xinf)) =>
          if (winningInf < xinf) (Set(xp), xinf)
          else if (winningInf == xinf && winningInf != 0.0) (winning + xp, winningInf)
          else (winning, winningInf)
      }
      F.pure(resolution._1)
    }
}
