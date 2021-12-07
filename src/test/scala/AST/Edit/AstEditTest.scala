package AST.Edit

import utest.{TestSuite, Tests, test}
import boopickle.Default._

object AstEditTest extends TestSuite {
  override def tests: Tests = Tests {
    test("Serialization") {
      val edit: AstEdit[Int] = AST.Edit.AstEdit.Move(10, 30, 6)
      val otw = Pickle.intoBytes(edit)
      val rcd = Unpickle[AstEdit[Int]].fromBytes(otw)
      assert(edit == rcd)
    }
  }
}
