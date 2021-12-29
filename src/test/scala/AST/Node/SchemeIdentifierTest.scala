package AST.Node

import utest.{TestSuite, Tests, test}
import AST.Node.SchemeNode._

object SchemeIdentifierTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeIdentifier operations") {
      val expression: SchemeIdentifier[Double] =
        SchemeIdentifier[Double](0, 0, 0, None, "Identifier")

      test("`sameLabel` should hold for expressions") {
        assert(expression sameLabel SchemeIdentifier[Double](0, 0, 0, None, "Identifier"))
        assert(!expression.sameLabel(SchemeNumber(0, 0, 0, None, 0)))
      }

      test("`sameValue` should only hold for identifier nodes with the same identifier") {
        assert(expression.sameValue(SchemeIdentifier[Double](123, 1234, 12345, None, "Identifier")))
        assert(!expression.sameValue(SchemeNumber(0, 0, 0, None, 0)))
      }

      test("`withParent` and `withValue` should replace the parent and value respectively") {
        assert(expression.withParent(123456).parent.contains(123456))
        assert(expression.withValue("newIdentifier").value == "newIdentifier".toSeq)
      }
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      val identifier = SchemeIdentifier(0, 1, 2, Some(3), "four")
      assert(SchemeIdentifier.unapply(identifier).get == (0, 1, 2, Some(3), "four".toSeq))
    }
  }
}
