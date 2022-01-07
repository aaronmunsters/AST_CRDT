package AST.GumTree

import AST.Node.SchemeNode._
import AST.Parse.Parser
import AST.TestUtils.getIdGenerator
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class PrioritySequenceTest extends AnyWordSpecLike with Matchers {

  "Printing works as expected" in {
    val Some(tree) = Parser.parse("(define a b c)", getIdGenerator)
    val ps = new PrioritySequence[Int](tree)
    assert(ps.toString == "PrioritySequence[]")
    ps.open(tree.root.map(tree.header).get)
    assert(ps.toString == "PrioritySequence[1-> define..., 2-> a..., 3-> b..., 4-> c...]")
    assert(ps.pop == Set(2, 3, 4, 1))
    assert(ps.pop == Set())

    ps.open(SchemeNumber(0, 0, 0, Some(0), 0))
    assert(ps.pop == Set())

    ps.push(tree.root.map(tree.header).get.asInstanceOf[RecursiveNode[Int]].children.head)
  }
}
