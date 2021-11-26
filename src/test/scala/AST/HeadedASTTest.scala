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
      val ast = HeadedAST.init[Identity]
      assert(true)

      val topLevelExpression = SchemeExpression(getIdentity, None, Seq())
      val plusIdentifier = SchemeIdentifier(getIdentity, topLevelExpression.parent, "+")
      val numberLeft = SchemeNumber(getIdentity, Some(topLevelExpression.id), 1)
      val numberRight = SchemeNumber(getIdentity, Some(topLevelExpression.id), 2)

      val firstEdit = Add(topLevelExpression, None, 0)
      val secondEdit = Add(plusIdentifier, Some(topLevelExpression.id), 0)
      val thirdEdit = Add(numberLeft, Some(topLevelExpression.id), 1)
      val fourthEdit = Add(numberRight, Some(topLevelExpression.id), 2)

      val astExpectedString = "(+ 1 2)"
      val astActualString = ast
        .perform(firstEdit)
        .perform(secondEdit)
        .perform(thirdEdit)
        .perform(fourthEdit)
        .toAstString

      println(astActualString)

      assert(astExpectedString == astActualString)
    }
  }
}
