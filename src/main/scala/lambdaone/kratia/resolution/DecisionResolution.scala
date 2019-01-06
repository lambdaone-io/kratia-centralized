package lambdaone.kratia.resolution

import lambdaone.kratia.collector.Proposal.BinaryProposal
import lambdaone.kratia.collector.InfluenceAllocation

sealed trait DecisionResolution {

  def resolve(allocation: InfluenceAllocation, maxInfluence: Double): Set[BinaryProposal]

}

object DecisionResolution {

  def apply(allocation: InfluenceAllocation, maxInfluence: Double, method: DecisionResolution): Set[BinaryProposal] =
    method.resolve(allocation, maxInfluence)

  case class LowInertia(inertia: Double) extends DecisionResolution {

    def resolve(allocation: InfluenceAllocation, maxInfluence: Double): Set[BinaryProposal] = {
      val requiredInfluence = maxInfluence * inertia
      val accumulated = allocation.value.foldLeft(0.0) {
        case (acc, (BinaryProposal(true), influence)) => acc + influence
        case (acc, (BinaryProposal(false), _)) => acc
      }
      if (accumulated >= requiredInfluence) Set(BinaryProposal(true)) else Set(BinaryProposal(false))
    }

  }

  object Majority extends DecisionResolution {

    def resolve(allocation: InfluenceAllocation, maxInfluence: Double): Set[BinaryProposal] = {
      val resolution = allocation.value.foldLeft((Set.empty[BinaryProposal], 0.0)) {
        case ((winning, winningInf), (xp, xinf)) =>
          if (winningInf < xinf) (Set(xp), xinf)
          else if (winningInf == xinf && winningInf != 0.0) (winning + xp, winningInf)
          else (winning, winningInf)
      }
      resolution._1
    }

  }
}
