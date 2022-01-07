package AST.Node

import AST.Node.SchemeNode._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SchemeNumberTest extends AnyWordSpecLike with Matchers {
  val expression: SchemeNumber[Double] =
    SchemeNumber[Double](0, 0, 0, None, 123456789)

  "`sameLabel` should hold for expressions" in {
    assert(expression sameLabel SchemeNumber[Double](0, 0, 0, None, 789456123))
    assert(!expression.sameLabel(SchemeIdentifier(0, 0, 0, None, "foobar")))
  }

  "`sameValue` should only hold for number nodes with the same number" in {
    assert(expression.sameValue(SchemeNumber[Double](0, 0, 0, None, 123456789)))
    assert(!expression.sameValue(SchemeIdentifier[Double](123, 1234, 12345, None, "Identifier")))
  }

  "`withParent` and `withValue` should replace the parent and value respectively" in {
    assert(expression.withParent(123456).parent.contains(123456))
    assert(expression.withValue(777777).value == 777777)
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val number = SchemeNumber(0, 1, 2, Some(3), 123456789)
    assert(SchemeNumber.unapply(number).get == (0, 1, 2, Some(3), 123456789))
  }
}
