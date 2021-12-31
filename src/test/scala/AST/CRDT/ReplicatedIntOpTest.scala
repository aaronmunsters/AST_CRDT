package AST.CRDT

import AST.CRDT.ReplicatedIntOp.{LClock, RId, NId}
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
        Some((RId(0), NId(1))),
      )

      val nodes: Seq[Option[(RId, NId)] => SchemeNode[(RId, NId)]] = Seq(
        (option: Option[(RId, NId)]) => SchemeExpression(0, 1, (RId(0), NId(2)), option, Seq((RId(0), NId(1)))),
        (option: Option[(RId, NId)]) => SchemeNumber(0, 1, (RId(0), NId(2)), option, 123456),
        (option: Option[(RId, NId)]) => SchemeIdentifier(0, 1, (RId(0), NId(2)), option, "foobar"),
        (option: Option[(RId, NId)]) => SchemeString(0, 1, (RId(0), NId(2)), option, "some string")
      )

      val operations: Seq[(SchemeNode[(RId, NId)], Option[(RId, NId)]) => AstEdit[(RId, NId)]] = Seq(
        (tree: SchemeNode[(RId, NId)], identity: Option[(RId, NId)]) => Add(tree, identity, 0),
        (_: SchemeNode[(RId, NId)], _: Option[(RId, NId)]) => Delete((RId(0), NId(0))),
        (_: SchemeNode[(RId, NId)], _: Option[(RId, NId)]) => Move((RId(0), NId(0)), (RId(0), NId(1)), 2),
        (_: SchemeNode[(RId, NId)], _: Option[(RId, NId)]) => UpdateValue((RId(0), NId(0)), 10L),
        (_: SchemeNode[(RId, NId)], _: Option[(RId, NId)]) => UpdateValue((RId(0), NId(0)), "HelloWorld"),
      )

      for {
        option <- options
        node <- nodes
        operation <- operations
      } {
        val original = ReplicatedIntOp((LClock(0), RId(1)), operation(node(option), option))
        val over_the_wire = ReplicatedIntOp.serialize(original)
        val received = ReplicatedIntOp.deserialize(over_the_wire)
        assert(original == received)
      }
    }

    test("Construction method") {
      assert(ReplicatedIntOp.from(ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0))))) == ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0)))))
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      val replicated = ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0))))
      assert(ReplicatedIntOp.unapply(replicated).get == ((LClock(0), RId(0)), Delete((RId(0), NId(0)))))
    }
  }
}
