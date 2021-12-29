package AST.Node
import AST.Node.SchemeNode._
import utest.{TestSuite, Tests, test}

object SchemeExpressionTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeExpression operations") {
      test("Creation") {
        // Through factory method
        SchemeExpression.empty(0, 0, 0)
        // Through case class constructor method
        SchemeExpression[Double](0, 0, 0, None, Seq(1, 2, 3, 4))
      }

      val expression: SchemeExpression[Double] =
        SchemeExpression[Double](0, 0, 0, None, Seq(1, 2, 3, 4))

      test("`contains` should indicate if subexpressions contain identities") {
        assert(expression.contains(1) && expression.contains(4))
      }

      test("`prependChild` should add a in front of all other children") {
        assert(expression.prependChild(99).children.head == 99)
      }

      test("`addChild` should respect the provided index, or add it to an empty otherwise") {
        assert(expression.addChild(2.5, 2).children == Seq(1, 2, 2.5, 3, 4))
        assert(SchemeExpression.empty(0, 0, 0).addChild(99, 99).children.contains(99))
      }

      test("`removeChild` should ensure the identity is not contained as a child afterwards") {
        assert(!expression.removeChild(3).contains(3))
      }

      test("`sameLabel` should hold for expressions") {
        assert(expression.sameLabel(SchemeExpression.empty(0, 0, 0)))
        assert(!expression.sameLabel(SchemeNumber(0, 0, 0, None, 0)))
      }

      test("`sameValue` should behave equally to sameLabel since expressions do not hold any values") {
        assert(expression.sameValue(SchemeExpression.empty(0, 0, 0)))
        assert(!expression.sameValue(SchemeNumber(0, 0, 0, None, 0)))
      }

      test("`withoutChildren` should equal to the provided argument without any children") {
        assert(expression.children.nonEmpty)
        assert(expression.withoutChildren.asInstanceOf[SchemeExpression[_]].children.isEmpty)
      }

      test("`toIdentifiedString` should include the identifier") {
        assert(SchemeExpression.empty(0, 0, 0).toIdentifiedString(null) == "(<0>--<0>)")
      }
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      val expression = SchemeExpression(0, 1, 2, Some(3), "four")
      assert(SchemeExpression.unapply(expression).get == (0, 1, 2, Some(3), "four".toSeq))
    }
  }
}
