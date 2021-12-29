package AST

import AST.CRDT.ReplicatedIntOp
import AST.Edit.AstEdit.{Add, Delete, Move}
import AST.Edit.UpdateValue
import AST.Node.SchemeNode
import AST.Node.SchemeNode.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}
import utest.{TestSuite, Tests, test}

object ReplicatedOperationTest extends TestSuite {
  override def tests: Tests = Tests {


    test("Serialization and deserialization") {
      val options = Seq(
        None,
        Some(0),
      )

      val nodes = Seq(
        (option: Option[Int]) => SchemeExpression(0,1,2,option,Seq(4,5,6)),
        (option: Option[Int]) => SchemeNumber(0,1,2,option,123456),
        (option: Option[Int]) => SchemeIdentifier(0,1,2,option, "foobar"),
        (option: Option[Int]) => SchemeString(0,1,2,option,"some string")
      )

      val operations = Seq(
        (tree: SchemeNode[Int], identity: Option[Int]) => Add(tree, identity, 0),
        (_: SchemeNode[Int], _: Option[Int]) => Delete(0),
        (_: SchemeNode[Int], _: Option[Int]) => Move(0, 1, 2),
        (_: SchemeNode[Int], _: Option[Int]) => UpdateValue(0, 10L),
        (_: SchemeNode[Int], _: Option[Int]) => UpdateValue(0, "HelloWorld"),
      )

      for {
        option <- options
        node <- nodes
        operation <- operations
      } {
        val original = ReplicatedIntOp((0,1), operation(node(option), option))
        println(original)
        val over_the_wire = ReplicatedIntOp.serialize(original)
        val received = ReplicatedIntOp.deserialize(over_the_wire)
        assert(original == received)
      }
    }

    test("Construction method") {
      assert(ReplicatedIntOp.from(ReplicatedIntOp((0,0), Delete(0))) == ReplicatedIntOp((0,0), Delete(0)))
    }

    // This helps to achieve 100% code coverage
    test("Case class behaviour as expected") {
      val replicated = ReplicatedIntOp((0,0), Delete(0))
      assert(ReplicatedIntOp.unapply(replicated).get == ((0,0), Delete(0)))
    }
  }
}
