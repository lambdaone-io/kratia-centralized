package lambdaone.toolbox

import org.scalatest.FunSuite
import cats.implicits._
import lambdaone.toolbox.denotations.CRUDStoreDenotation
import lambdaone.toolbox.discipline.CRUDStoreTests
import org.typelevel.discipline.scalatest.Discipline

class CRUDStoreSpec extends FunSuite with Discipline {

  checkAll("CRUDStore[Denotation[Int, ?], Int, Int]", CRUDStoreTests(
    CRUDStoreDenotation[Int, Int],
    CRUDStoreDenotation.Interpreter[Int, Int]
  ).crud)

}
