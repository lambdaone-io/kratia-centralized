package lambdaone.toolbox

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.FunSuite
import cats.instances.string._
import lambdaone.toolbox.denotations.CRUDStoreDenotation
import lambdaone.toolbox.discipline.CRUDStoreTests
import lambdaone.toolbox.mem.CRUDStoreInMem
import org.typelevel.discipline.scalatest.Discipline

class CRUDStoreSpec extends FunSuite with Discipline {

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
}
