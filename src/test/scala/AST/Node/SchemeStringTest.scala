package AST.Node

import AST.Node.SchemeNode._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SchemeStringTest extends AnyWordSpecLike with Matchers {

  val expression: SchemeString[Double] =
    SchemeString[Double](0, 0, 0, None, "foo bar baz")

  "`sameLabel` should hold for expressions" in {
    assert(expression sameLabel (expression withValue "value"))
    assert(!(expression sameLabel SchemeIdentifier(0, 0, 0, None, "foobar")))
  }

  "`sameValue` should only hold for string nodes with the same string content" in {
    assert(expression.sameValue(SchemeString[Double](10, 20, 30, None, "foo bar baz")))
    assert(!expression.sameValue(SchemeString[Double](10, 20, 30, None, "foo foo foo")))
  }

  "`withParent` and `withValue` should replace the parent and value respectively" in {
    assert(expression.withParent(123456).parent.contains(123456))
    assert(expression.withValue("new foo").value == "new foo".toSeq)
  }

  "`toAstString` should include the quotation marks" in {
    assert(expression.toAstString(null) == "\"foo bar baz\"")
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val string = SchemeString(0, 1, 2, Some(3), "123456789")
    assert(SchemeString.unapply(string).get == (0, 1, 2, Some(3), "123456789".toSeq))
  }
}
