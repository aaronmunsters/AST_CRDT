package AST.Node

import utest.{TestSuite, Tests, test}

object SchemeExpressionTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeExpression operations") {
      val expression = SchemeExpression[Double](0, None, Seq(1, 2, 3, 4))

      test("`contains` should indicate if subexpressions contain identities"){
        assert(expression.contains(1) && expression.contains(4))
      }

      test("`prependChild` should add a in front of all other children") {
        assert(expression.prependChild(99).subexpressions.head == 99)
      }

      test("`addChild` should respect the provided index"){
        assert(expression.addChild(2.5, 2).subexpressions == Seq(1, 2, 2.5, 3, 4))
      }

      test("`removeChild` should ensure the identity is not contained as a child afterwards"){
        assert(!expression.removeChild(3).contains(3))
      }
    }
  }
}
