package lambdaone.kratia.collector

case class Vote[Address, P](member: Address, influenceAllocation: InfluenceAllocation[P])
