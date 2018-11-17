package lambdaone.toolbox

import cats.implicits._
import cats.data.NonEmptyList
import lambdaone.collector.Collector.Ballot
import lambdaone.toolbox.denotations.CollectorDenotation
import lambdaone.toolbox.discipline.CollectorTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class CollectorSpec extends FunSuite with Discipline {

  implicit val genBallot: Arbitrary[Ballot[Boolean]] = Arbitrary(Gen.oneOf(
    Ballot(NonEmptyList.fromListUnsafe(List(false, true))),
    Ballot(NonEmptyList.fromListUnsafe(List(true, false)))
  ))

  checkAll("Collector[Denotation[String, ?], Int, String]", CollectorTests(
    CollectorDenotation[Int, Boolean],
    CollectorDenotation.Interpreter[Int, Boolean]
  ).collector)
}
