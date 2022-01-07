package AST.CRDT

import AST.CRDT.ReplicatedIntOp.{LClock, NId, RId}
import AST.Edit.AstEdit.{Add, Delete, Move}
import AST.Edit.{AstEdit, UpdateValue}
import AST.Node.SchemeNode
import AST.Node.SchemeNode.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ReplicatedIntOpTest extends AnyWordSpecLike with Matchers {

  "Serialization and deserialization" in {
    val options = Seq(
      None,
      Some((RId(0), NId(1))),
    )

    val nodes: Seq[Option[(RId, NId)] => SchemeNode[(RId, NId)]] = Seq(
      SchemeExpression(0, 1, (RId(0), NId(2)), _, Seq((RId(0), NId(1)))),
      SchemeNumber(0, 1, (RId(0), NId(2)), _, 123456),
      SchemeIdentifier(0, 1, (RId(0), NId(2)), _, "foobar"),
      SchemeString(0, 1, (RId(0), NId(2)), _, "some string")
    )

    val operations: Seq[(SchemeNode[(RId, NId)], Option[(RId, NId)]) => AstEdit[(RId, NId)]] = Seq(
      (tree, identity) => Add(tree, identity, 0),
      (_, _) => Delete((RId(0), NId(0))),
      (_, _) => Move((RId(0), NId(0)), (RId(0), NId(1)), 2),
      (_, _) => UpdateValue((RId(0), NId(0)), 10L),
      (_, _) => UpdateValue((RId(0), NId(0)), "HelloWorld"),
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

  "Construction method" in {
    assert(ReplicatedIntOp.from(ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0))))) == ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0)))))
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val replicated = ReplicatedIntOp((LClock(0), RId(0)), Delete((RId(0), NId(0))))
    assert(ReplicatedIntOp.unapply(replicated).get == ((LClock(0), RId(0)), Delete((RId(0), NId(0)))))
  }

}
