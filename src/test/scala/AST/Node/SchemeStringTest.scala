package AST.Node

import utest.{TestSuite, Tests, test}

object SchemeStringTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeStringTest operations") {
      val expression: SchemeString[Double] =
        SchemeString[Double](0, 0, 0, None, "foo bar baz")

      test("`sameLabel` should hold for expressions") {
        assert(expression sameLabel (expression withValue "value"))
        assert(!(expression sameLabel SchemeIdentifier(0, 0, 0, None, "foobar")))
      }

      test("`sameValue` should only hold for string nodes with the same string content") {
        assert(expression.sameValue(SchemeString[Double](10, 20, 30, None, "foo bar baz")))
        assert(!expression.sameValue(SchemeString[Double](10, 20, 30, None, "foo foo foo")))
      }

      test("`withParent` and `withValue` should replace the parent and value respectively") {
        assert(expression.withParent(123456).parent.contains(123456))
        assert(expression.withValue("new foo").value == "new foo")
      }

      test("`toAstString` should include the quotation marks") {
        assert(expression.toAstString(null) == "\"foo bar baz\"")
      }
    }

    test("Case class behaviour as expected") {
      val string = SchemeString(0, 1, 2, Some(3), "123456789")
      assert(SchemeString.unapply(string).get == (0, 1, 2, Some(3), "123456789"))
    }
  }
}
