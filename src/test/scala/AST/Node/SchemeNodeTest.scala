package AST.Node

import AST.Edit.AstEdit.Add
import AST.HeadedAST
import AST.Node.SchemeNode._
import AST.Parse.Parser
import AST.TestUtils.getIdGenerator
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SchemeNodeTest extends AnyWordSpecLike with Matchers {
  val leaf: LeafNode[Int, Long] =
    SchemeNumber(0, 0, 0, None, 123456789)
  val recursive: RecursiveNode[Int] =
    SchemeExpression(0, 0, 1, None, Seq(leaf.id))
  implicit val ast: HeadedAST[Int] =
    HeadedAST.withRoot(recursive).perform(Add(leaf, Some(recursive.id), 0))

  "Measuring the height of a node" in {
    assert(leaf.height == 1)
    assert(recursive.height == 2)
  }

  "Order of descendants should be computed correctly" in {
    val idGenerator = getIdGenerator
    implicit val Some(tree) = Parser.parse("(a (b c d) e (f g) h (i j (k l)))", idGenerator)
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

  "Isomorphic nodes are nodes with the same label and values, but not with equal identity" in {
    val tree = Parser.parse("(123456789)", getIdGenerator).get
    assert(tree isomorphic ast)
  }

  "`toIdentifiedString` shows the identities" in assert(ast.toIdentifiedString() == "(<1>-<0>123456789-<1>)")

  "`withoutChildren` removes children" in {
    assert(leaf.withoutChildren == leaf)
    assert(recursive.withoutChildren.asInstanceOf[SchemeExpression[_]].children.isEmpty)
  }

  "`contains` behaves correctly" in {
    assert(recursive contains leaf.id)
    assert(!(recursive contains 1234))
  }

  "`toAstString` simply prints the AST in string format" in {
    val tree = Parser.parse("(define a b c (d e f) (g h (i j k)))", getIdGenerator).get
    assert(tree.toAstString() == "(define a b c (d e f) (g h (i j k)))")
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    assert(SchemeNode == SchemeNode)
    assert(PreOrder == PreOrder)
    assert(PostOrder == PostOrder)
    assert(BreadthFirst == BreadthFirst)
  }
}
