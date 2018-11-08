package lambdaone.toolbox

import java.util.UUID

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.FunSuite
import cats.instances.int._
import lambdaone.toolbox.denotations.CRUDStoreDenotation
import lambdaone.toolbox.discipline.CRUDStoreTests
import lambdaone.toolbox.mem.CRUDStoreInMem
import org.typelevel.discipline.scalatest.Discipline

class CRUDStoreSpec extends FunSuite with Discipline {

  checkAll("CRUDStore[Denotation[Int, ?], Int, Int]", CRUDStoreTests(
    CRUDStoreDenotation[Int, Int],
    CRUDStoreDenotation.Interpreter[Int, Int]
  ).crud)

  checkAll("CRUDStore[InMem[Int, ?], UUID, Int]", CRUDStoreTests(
    CRUDStoreInMem[Int],
    CRUDStoreInMem.Interpreter[Int]
  ).crud)
}
