package lambdaone.kratia.resolution

import cats.Applicative
import lambdaone.kratia.collector.BinaryProposal
import lambdaone.kratia.collector.BinaryProposal.{Yes, No}

case class LowInertia(inertia: Double)

object LowInertia {

  implicit def lowInertia[F[_]](implicit F: Applicative[F]): DecisionResolution[F, BinaryProposal, LowInertia] =
    (allocation, maxInfluence, lowInertia) => {
      val requiredInfluence = maxInfluence * lowInertia.inertia
      val accumulated = allocation.value.foldLeft(0.0) {
        case (acc, (Yes, influence)) => acc + influence
        case (acc, (No, _)) => acc
      }
      F.pure(if(accumulated >= requiredInfluence) Set(Yes) else Set(No))
    }
}
