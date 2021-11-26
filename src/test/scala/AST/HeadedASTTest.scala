package AST

import AST.Edit.{Add, Delete}
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber}
import utest._

object HeadedASTTest extends TestSuite {
  type Identity = Int
  var uniqueId: Identity = 0

  private def getIdentity = {
    uniqueId += 1; uniqueId
  }

  override def tests: Tests = Tests {
    test("Creation") {
      val ast = HeadedAST.empty[Identity]
      assert(true)

      val plusWrapExpression = SchemeExpression(getIdentity, None, Seq())
      val plusIdentifier = SchemeIdentifier(getIdentity, Some(plusWrapExpression.id), "+")
      val numberLeft = SchemeNumber(getIdentity, Some(plusWrapExpression.id), 1)
      val numberRight = SchemeNumber(getIdentity, Some(plusWrapExpression.id), 2)

      val astExpectedString = "(+ 1 2)"
      val astWithSimpleAddition = ast
        .perform(Add.from(plusWrapExpression))
        .perform(Add.from(plusIdentifier))
        .perform(Add.from(numberLeft))
        .perform(Add.from(numberRight))

      assert(astExpectedString == astWithSimpleAddition.toAstString)

      assert(astWithSimpleAddition.perform(Delete(plusIdentifier.id)).toAstString == "(1 2)")

      val emptyAst = astWithSimpleAddition.perform(Delete(plusWrapExpression))
      println(emptyAst.toAstString)
    }
  }
}
