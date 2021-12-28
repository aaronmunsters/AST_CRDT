package AST.Node

import AST.Edit.AstEdit.Add
import AST.HeadedAST
import AST.Node.SchemeNode._
import AST.Parse.Parser
import AST.TestUtils.getIdGenerator
import utest.{TestSuite, Tests, test}

object SchemeNodeTest extends TestSuite {
  override def tests: Tests = Tests {
    test("SchemeNodeTest operations") {
      val leaf: LeafNode[Int, Long] =
        SchemeNumber(0, 0, 0, None, 123456789)
      val recursive: RecursiveNode[Int] =
        SchemeExpression(0, 0, 1, None, Seq(leaf.id))
      implicit val ast: HeadedAST[Int] =
        HeadedAST.withRoot(recursive).perform(Add(leaf, Some(recursive.id), 0))

      test("Measuring the height of a node") {
        assert(leaf.height == 1)
        assert(recursive.height == 2)
      }

      test("Order of descendants should be computed correctly") {
        val idGenerator = getIdGenerator
        implicit val Some(tree) = Parser.parseSchemeSmall("(a (b c d) e (f g) h (i j (k l)))", idGenerator)
        val Some(root_id) = tree.root

        def get_identifiers_in(order: TraverseOrder) = {
          val sample_identifier = SchemeIdentifier(0, 0, idGenerator(), None, "empty")
          tree(root_id)
            .descendants(order)(tree)
            .map(tree(_))
            .filter(sample_identifier.sameLabel)
            .map(_.toAstString(tree))
        }

        assert(get_identifiers_in(PreOrder) == Seq("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
        assert(get_identifiers_in(BreadthFirst) == Seq("a", "e", "h", "b", "c", "d", "f", "g", "i", "j", "k", "l"))

        // difference is in relation to children, but identifiers are never parents
        assert(get_identifiers_in(PostOrder) == Seq("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
      }

      test("Isomorphic nodes are nodes with the same label and values, but not with equal identity") {
        val tree = Parser.parseSchemeSmall("(123456789)", getIdGenerator).get
        assert(tree isomorphic ast)
      }

      test("`toIdentifiedString` shows the identities") - assert(ast.toIdentifiedString() == "(<1>-<0>123456789-<1>)")

      test("`withoutChildren` removes children") {
        assert(leaf.withoutChildren == leaf)
        assert(recursive.withoutChildren.asInstanceOf[SchemeExpression[_]].children.isEmpty)
      }

      test("`contains` behaves correctly") {
        assert(recursive contains leaf.id)
        assert(!(recursive contains 1234))
      }

      test("`toAstString` simply prints the AST in string format") {
        val tree = Parser.parseSchemeSmall("(define a b c (d e f) (g h (i j k)))", getIdGenerator).get
        assert(tree.toAstString() == "(define a b c (d e f) (g h (i j k)))")
      }
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      assert(SchemeNode == SchemeNode)
      assert(PreOrder == PreOrder)
      assert(PostOrder == PostOrder)
      assert(BreadthFirst == BreadthFirst)
    }
  }
}
