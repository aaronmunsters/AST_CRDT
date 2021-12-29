package AST.Edit

import AST.Node.SchemeNumber
import AST.Parse.Parser
import AST.TestUtils
import utest.{TestSuite, Tests, test}
import boopickle.Default._
import AST.Edit.AstEdit._
import AST.Node.SchemeNode.RecursiveNode

object AstEditTest extends TestSuite {
  override def tests: Tests = Tests {
    test("Serialization") {
      val edit: AstEdit[Int] = AST.Edit.AstEdit.Move(10, 30, 6)
      val otw = Pickle.intoBytes(edit)
      val rcd = Unpickle[AstEdit[Int]].fromBytes(otw)
      assert(edit == rcd)
    }

    test("Serialization") {
      val idGenerator = TestUtils.getIdGenerator
      val Some(tree) = Parser.parse("(define a b c)", idGenerator)
      val number = SchemeNumber(0, 0, idGenerator(), Some(tree.root.get), 123456)
      val define_id = tree(tree.root.get).asInstanceOf[RecursiveNode[Int]].children.head
      val updated = tree
        .perform(AST.Edit.Add.from(number))
        .perform(UpdateValue(number.id, 1234567L))
        .perform(UpdateValue(define_id, "DEFINE"))
        .perform(Move(number.id, tree.root.get, 0))
        .perform(AST.Edit.Delete(number))
    }

    // This helps to achieve 100% code coverage
    test("Object equality") {
      assert(AST.Edit.Add == AST.Edit.Add)
      assert(AST.Edit.Delete == AST.Edit.Delete)
      assert(AST.Edit.Move == AST.Edit.Move)
      assert(AST.Edit.UpdateValue == AST.Edit.UpdateValue)
      assert(Add == Add)
      assert(Delete == Delete)
      assert(Move == Move)
      assert(UpdateString == UpdateString)
      assert(UpdateNumber == UpdateNumber)
      assert(AstEdit == AstEdit)

      assert(Add.unapply(Add(null, null, 0)).get == (null, null, 0))
      assert(Delete.unapply(Delete(null, null)).get == (null, null))
      assert(Move.unapply(Move(null, null, 0)).get == (null, null, 0))
      assert(UpdateString.unapply(UpdateString(null, null)).get == (null, null))
      assert(UpdateNumber.unapply(UpdateNumber(null, 0)).get == (null, 0))
    }
  }
}
