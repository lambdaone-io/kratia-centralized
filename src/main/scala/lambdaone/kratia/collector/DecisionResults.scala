package lambdaone.kratia.collector

import java.util.UUID

case class DecisionResults (address: UUID, closedOn: Timestamp, data: DecisionData, maxInfluence: Double, allocation: InfluenceAllocation)

