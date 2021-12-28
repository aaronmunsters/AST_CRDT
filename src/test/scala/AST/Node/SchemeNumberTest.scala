package AST.Node

import utest.{TestSuite, Tests, test}

object SchemeNumberTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeNumberTest operations") {
      val expression: SchemeNumber[Double] =
        SchemeNumber[Double](0, 0, 0, None, 123456789)

      test("`sameLabel` should hold for expressions") {
        assert(expression sameLabel SchemeNumber[Double](0, 0, 0, None, 789456123))
        assert(!expression.sameLabel(SchemeIdentifier(0, 0, 0, None, "foobar")))
      }

      test("`sameValue` should only hold for number nodes with the same number") {
        assert(expression.sameValue(SchemeNumber[Double](0, 0, 0, None, 123456789)))
        assert(!expression.sameValue(SchemeIdentifier[Double](123, 1234, 12345, None, "Identifier")))
      }

      test("`withParent` and `withValue` should replace the parent and value respectively") {
        assert(expression.withParent(123456).parent.contains(123456))
        assert(expression.withValue(777777).value == 777777)
      }
    }

    test("Case class behaviour as expected") {
      val number = SchemeNumber(0, 1, 2, Some(3), 123456789)
      assert(SchemeNumber.unapply(number).get == (0, 1, 2, Some(3), 123456789))
    }
  }
}
