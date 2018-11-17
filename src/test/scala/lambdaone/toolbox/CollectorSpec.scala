package lambdaone.toolbox

import lambdaone.collector.Collector
import lambdaone.collector.Collector.{Ballot, BinaryProposal}
import lambdaone.toolbox.denotations.CollectorDenotation
import lambdaone.toolbox.discipline.CollectorTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class CollectorSpec extends FunSuite with Discipline {

  implicit val gen1: Arbitrary[BinaryProposal] = Arbitrary(
     for {x <- Gen.oneOf(Collector.binaryBallot, Collector.binaryBallot)} yield x
  )

  checkAll("Collector[Denotation[String, ?], Int, String]", CollectorTests(
    CollectorDenotation[Int,  BinaryProposal],
    CollectorDenotation.Interpreter[Int, BinaryProposal]
  ).collector)
}
