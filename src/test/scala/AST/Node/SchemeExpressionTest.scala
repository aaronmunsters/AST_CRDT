package AST.Node

import AST.Node.SchemeNode.{BreadthFirst, PostOrder, PreOrder, TraverseOrder}
import AST.Parse.Parser
import utest.{TestSuite, Tests, test}

object SchemeExpressionTest extends TestSuite {
  private def getIdGenerator = {
    var id = 0
    () => {
      id += 1
      id
    }
  }

  override def tests: Tests = Tests {
    test("SchemeExpression operations") {
      val expression = SchemeExpression[Double](0, None, Seq(1, 2, 3, 4))

      test("`contains` should indicate if subexpressions contain identities") {
        assert(expression.contains(1) && expression.contains(4))
      }

      test("`prependChild` should add a in front of all other children") {
        assert(expression.prependChild(99).children.head == 99)
      }

      test("`addChild` should respect the provided index") {
        assert(expression.addChild(2.5, 2).children == Seq(1, 2, 2.5, 3, 4))
      }

      test("`removeChild` should ensure the identity is not contained as a child afterwards") {
        assert(!expression.removeChild(3).contains(3))
      }

      test("Descendants should be computed correctly") {
        val idGenerator = getIdGenerator
        implicit val Some(tree) = Parser.parseSchemeSmall("(a (b c d) e (f g) h (i j (k l)))", idGenerator)
        val Some(root_id) = tree.root

        def get_identifiers_in(order: TraverseOrder) = {
          val sample_identifier = SchemeIdentifier(idGenerator(), None, "empty")
          tree(root_id)
            .descendants(order)
            .map(tree(_))
            .filter(sample_identifier.sameLabel)
            .map(_.toAstString)
        }

        assert(get_identifiers_in(PreOrder) == Seq("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
        assert(get_identifiers_in(BreadthFirst) == Seq("a", "e", "h", "b", "c", "d", "f", "g", "i", "j", "k", "l"))

        // difference is in relation to children, but identifiers are never parents
        assert(get_identifiers_in(PostOrder) == Seq("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
      }
    }
  }
}
