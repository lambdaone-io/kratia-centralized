package lambdaone.toolbox

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import lambdaone.collector.Collector.{Ballot, BallotBox}
import lambdaone.toolbox.denotations.{CRUDStoreDenotation, CollectorDenotation}
import lambdaone.toolbox.discipline.{CRUDStoreTests, CollectorTests}
import lambdaone.toolbox.mem.CRUDStoreInMem
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class CollectorSpec extends FunSuite with Discipline {

  implicit val genBallot: Arbitrary[Ballot[Boolean]] = Arbitrary(Gen.oneOf(
    Ballot(NonEmptyList.fromListUnsafe(List(false, true))),
    Ballot(NonEmptyList.fromListUnsafe(List(true, false)))
  ))

  checkAll("Collector...", CollectorTests[
    CollectorInMem.InMem[Int, BallotBox[Int, Boolean]], ?],
    CRUDStoreDenotation.Denotation[String, String, ?],
    String,
    String
    ](
    implementation = CRUDStoreInMem[String, String](UniqueGen.UniqueGenUUID[IO].map(_.toString)),
    denotation = CRUDStoreDenotation[String, String](),
    implement = CRUDStoreInMem.runUnsafeSync,
    denote = CRUDStoreDenotation.run(Map.empty[String, String] -> "0", x => (x.toInt + 1).toString)
  ).crud)

 /*
   checkAll("CRUDStore[InMem[String, String, ?], String, String]", CRUDStoreTests[
    CRUDStoreInMem.InMem[String, String, ?],
    CRUDStoreDenotation.Denotation[String, String, ?],
    String,
    String
  ](
    implementation = CRUDStoreInMem[String, String](UniqueGen.UniqueGenUUID[IO].map(_.toString)),
    denotation = CRUDStoreDenotation[String, String](),
    implement = CRUDStoreInMem.runUnsafeSync,
    denote = CRUDStoreDenotation.run(Map.empty[String, String] -> "0", x => (x.toInt + 1).toString)
  ).crud)
  */
}
