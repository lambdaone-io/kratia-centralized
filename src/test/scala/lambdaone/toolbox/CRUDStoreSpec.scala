package lambdaone.toolbox

import org.scalatest.FunSuite
import cats.instances.string._
import lambdaone.toolbox.denotations.CRUDStoreDenotation
import lambdaone.toolbox.discipline.CRUDStoreTests
import lambdaone.toolbox.mem.CRUDStoreInMem
import org.typelevel.discipline.scalatest.Discipline

class CRUDStoreSpec extends FunSuite with Discipline {

  checkAll("CRUDStore[Denotation[String, ?], Int, String]", CRUDStoreTests(
    CRUDStoreDenotation[Int, String],
    CRUDStoreDenotation.Interpreter[Int, String]
  ).crud)

  checkAll("CRUDStore[InMem[String, ?], UUID, String]", CRUDStoreTests(
    CRUDStoreInMem[String],
    CRUDStoreInMem.Interpreter[String]
  ).crud)
}
