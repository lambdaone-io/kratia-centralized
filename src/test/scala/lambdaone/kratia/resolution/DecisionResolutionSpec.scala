package lambdaone.kratia.resolution

import cats.Id
import lambdaone.kratia.collector.BinaryProposal.{No, Yes}
import lambdaone.kratia.collector.{BinaryProposal, InfluenceAllocation}
import org.scalatest.FlatSpec

class DecisionResolutionSpec extends FlatSpec {

  val allocation1: InfluenceAllocation[BinaryProposal] =
    InfluenceAllocation(Map(
      Yes -> 51.0,
      No -> 49.0
    ))

  val allocation2: InfluenceAllocation[BinaryProposal] =
    InfluenceAllocation(Map(
      Yes -> 80.0,
      No -> 20.0
    ))

  "Majority" should "work" in {
    val result1 = DecisionResolution.resolve[Id, BinaryProposal, Majority](allocation1, 100.0, Majority())
    val result2 = DecisionResolution.resolve[Id, BinaryProposal, Majority](allocation2, 100.0, Majority())
    assert(result1 == Set(Yes))
    assert(result2 == Set(Yes))
  }

  "LowInertia" should "work" in {
    val result1 = DecisionResolution.resolve[Id, BinaryProposal, LowInertia](allocation1, 100.0, LowInertia(0.6))
    val result2 = DecisionResolution.resolve[Id, BinaryProposal, LowInertia](allocation2, 100.0, LowInertia(0.6))
    assert(result1 == Set(No))
    assert(result2 == Set(Yes))
  }
}
