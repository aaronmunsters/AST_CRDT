package AST

import AST.Edit.Add
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

      val topLevelExpression = SchemeExpression(getIdentity, None, Seq())
      val plusIdentifier = SchemeIdentifier(getIdentity, Some(topLevelExpression.id), "+")
      val numberLeft = SchemeNumber(getIdentity, Some(topLevelExpression.id), 1)
      val numberRight = SchemeNumber(getIdentity, Some(topLevelExpression.id), 2)

      val astExpectedString = "(+ 1 2)"
      val astActualString = ast
        .perform(Add.from(topLevelExpression))
        .perform(Add.from(plusIdentifier))
        .perform(Add.from(numberLeft))
        .perform(Add.from(numberRight))
        .toAstString

      assert(astExpectedString == astActualString)
    }
  }
}
