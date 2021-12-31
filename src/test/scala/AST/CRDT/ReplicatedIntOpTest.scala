package AST.CRDT

import AST.Edit.AstEdit.{Add, Delete, Move}
import AST.Edit.{AstEdit, UpdateValue}
import AST.Node.SchemeNode
import AST.Node.SchemeNode.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}
import utest.{TestSuite, Tests, test}

object ReplicatedIntOpTest extends TestSuite {
  override def tests: Tests = Tests {


    test("Serialization and deserialization") {
      val options = Seq(
        None,
        Some((0, 1)),
      )

      val nodes: Seq[Option[(Int, Int)] => SchemeNode[(Int, Int)]] = Seq(
        (option: Option[(Int, Int)]) => SchemeExpression(0, 1, (0, 2), option, Seq((0, 4), (0, 5), (0, 6))),
        (option: Option[(Int, Int)]) => SchemeNumber(0, 1, (0, 2), option, 123456),
        (option: Option[(Int, Int)]) => SchemeIdentifier(0, 1, (0, 2), option, "foobar"),
        (option: Option[(Int, Int)]) => SchemeString(0, 1, (0, 2), option, "some string")
      )

      val operations: Seq[(SchemeNode[(Int, Int)], Option[(Int, Int)]) => AstEdit[(Int, Int)]] = Seq(
        (tree: SchemeNode[(Int, Int)], identity: Option[(Int, Int)]) => Add(tree, identity, 0),
        (_: SchemeNode[(Int, Int)], _: Option[(Int, Int)]) => Delete((0, 0)),
        (_: SchemeNode[(Int, Int)], _: Option[(Int, Int)]) => Move((0, 0), (0, 1), 2),
        (_: SchemeNode[(Int, Int)], _: Option[(Int, Int)]) => UpdateValue((0, 0), 10L),
        (_: SchemeNode[(Int, Int)], _: Option[(Int, Int)]) => UpdateValue((0, 0), "HelloWorld"),
      )

      for {
        option <- options
        node <- nodes
        operation <- operations
      } {
        val original = ReplicatedIntOp((0, 1), operation(node(option), option))
        val over_the_wire = ReplicatedIntOp.serialize(original)
        val received = ReplicatedIntOp.deserialize(over_the_wire)
        assert(original == received)
      }
    }

    test("Construction method") {
      assert(ReplicatedIntOp.from(ReplicatedIntOp((0, 0), Delete((0, 0)))) == ReplicatedIntOp((0, 0), Delete((0, 0))))
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      val replicated = ReplicatedIntOp((0, 0), Delete((0, 0)))
      assert(ReplicatedIntOp.unapply(replicated).get == ((0, 0), Delete((0, 0))))
    }
  }
}
