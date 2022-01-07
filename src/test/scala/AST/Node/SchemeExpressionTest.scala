package AST.Node

import AST.Node.SchemeNode._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SchemeExpressionTest extends AnyWordSpecLike with Matchers {

  "Creation" in {
    // Through factory method
    SchemeExpression.empty(0, 0, 0)
    // Through case class constructor method
    SchemeExpression[Double](0, 0, 0, None, Seq(1, 2, 3, 4))
  }

  val expression: SchemeExpression[Double] =
    SchemeExpression[Double](0, 0, 0, None, Seq(1, 2, 3, 4))

  "`contains` should indicate if subexpressions contain identities" in {
    assert(expression.contains(1) && expression.contains(4))
  }

  "`prependChild` should add a in front of all other children" in {
    assert(expression.prependChild(99).children.head == 99)
  }

  "`addChild` should respect the provided index, or add it to an empty otherwise" in {
    assert(expression.addChild(2.5, 2).children == Seq(1, 2, 2.5, 3, 4))
    assert(SchemeExpression.empty(0, 0, 0).addChild(99, 99).children.contains(99))
  }

  "`removeChild` should ensure the identity is not contained as a child afterwards" in {
    assert(!expression.removeChild(3).contains(3))
  }

  "`sameLabel` should hold for expressions" in {
    assert(expression.sameLabel(SchemeExpression.empty(0, 0, 0)))
    assert(!expression.sameLabel(SchemeNumber(0, 0, 0, None, 0)))
  }

  "`sameValue` should behave equally to sameLabel since expressions do not hold any values" in {
    assert(expression.sameValue(SchemeExpression.empty(0, 0, 0)))
    assert(!expression.sameValue(SchemeNumber(0, 0, 0, None, 0)))
  }

  "`withoutChildren` should equal to the provided argument without any children" in {
    assert(expression.children.nonEmpty)
    assert(expression.withoutChildren.asInstanceOf[SchemeExpression[_]].children.isEmpty)
  }

  "`toIdentifiedString` should include the identifier" in {
    assert(SchemeExpression.empty(0, 0, 0).toIdentifiedString(null) == "(<0>--<0>)")
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val expression = SchemeExpression(0, 1, 2, Some(3), "four")
    assert(SchemeExpression.unapply(expression).get == (0, 1, 2, Some(3), "four".toSeq))
  }
}
