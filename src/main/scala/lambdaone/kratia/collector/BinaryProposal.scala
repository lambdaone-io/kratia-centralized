package lambdaone.kratia.collector

import cats.data.NonEmptyList

sealed abstract class BinaryProposal(val repr: String) {

  override def toString: String = repr
}

object BinaryProposal {

  object Yes extends BinaryProposal("yes")

  object No extends BinaryProposal("no")

  val AllChoices: NonEmptyList[BinaryProposal] = NonEmptyList.fromListUnsafe(List(Yes, No))

  val ballot: Ballot[BinaryProposal] = Ballot[BinaryProposal](BinaryProposal.AllChoices)

}


