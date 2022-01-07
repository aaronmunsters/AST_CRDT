package AST.Node

import AST.Node.SchemeNode._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SchemeIdentifierTest extends AnyWordSpecLike with Matchers {
  val expression: SchemeIdentifier[Double] =
    SchemeIdentifier[Double](0, 0, 0, None, "Identifier")

  "`sameLabel` should hold for expressions" in {
    assert(expression sameLabel SchemeIdentifier[Double](0, 0, 0, None, "Identifier"))
    assert(!expression.sameLabel(SchemeNumber(0, 0, 0, None, 0)))
  }

  "`sameValue` should only hold for identifier nodes with the same identifier" in {
    assert(expression.sameValue(SchemeIdentifier[Double](123, 1234, 12345, None, "Identifier")))
    assert(!expression.sameValue(SchemeNumber(0, 0, 0, None, 0)))
  }

  "`withParent` and `withValue` should replace the parent and value respectively" in {
    assert(expression.withParent(123456).parent.contains(123456))
    assert(expression.withValue("newIdentifier").value == "newIdentifier".toSeq)
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val identifier = SchemeIdentifier(0, 1, 2, Some(3), "four")
    assert(SchemeIdentifier.unapply(identifier).get == (0, 1, 2, Some(3), "four".toSeq))
  }
}
