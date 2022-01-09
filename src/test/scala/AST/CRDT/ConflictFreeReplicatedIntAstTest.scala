package AST.CRDT

import AST.CRDT.ReplicatedIntOp.RId
import AST.TX
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.mutable

class ConflictFreeReplicatedIntAstTest extends AnyWordSpecLike with Matchers {
  class MockedTransmitter(network: scala.collection.mutable.Set[MockedTransmitter]) extends TX[ReplicatedIntOp] {
    var callback: Option[Seq[ReplicatedIntOp] => Unit] = None
    // add transmitter to network
    network.add(this)

    override def publish(value: Seq[ReplicatedIntOp]): Unit =
      network.filter(_ != this).foreach(_.receive(value))

    override def subscribe(callback: Seq[ReplicatedIntOp] => Unit): Unit =
      this.callback = Some(callback)

    def receive(operations: Seq[ReplicatedIntOp]): Unit = {
      assert(callback.nonEmpty)
      callback.foreach(_ (operations))
    }
  }

  "Test the replicated ASTs" in {
    val network: mutable.Set[MockedTransmitter] = mutable.Set.empty
    val CFR_IA_1___ = ConflictFreeReplicatedIntAst(RId(1), new MockedTransmitter(network))
    val CFR_IA____2 = ConflictFreeReplicatedIntAst(RId(2), new MockedTransmitter(network))

    def assert_equality(): Unit = {
      assert(CFR_IA_1___.query isomorphic CFR_IA____2.query)
      assert(CFR_IA_1___.operations == CFR_IA____2.operations)
    }

    assert(CFR_IA_1___.query.toAstString() == CFR_IA____2.query.toAstString())

    CFR_IA_1___.update(0, "( define           )")
    assert_equality()
    CFR_IA____2.update(0, "( define a    b c  )")
    assert_equality()
    CFR_IA_1___.update(0, "( define a    b ccc)")
    assert_equality()
    CFR_IA____2.update(0, "((define c)   b a d)")
    assert_equality()
    CFR_IA_1___.update(0, "((define c)     a d)")
    assert_equality()
    CFR_IA____2.update(0, "((define) (foo bar))")
    assert_equality()

    val operations = CFR_IA_1___.operations

    val rnd = scala.util.Random
    scala.util.Random.setSeed(0L)

    for (operations_subset <- operations.toSet.subsets()) {
      val network: mutable.Set[MockedTransmitter] = mutable.Set.empty
      val CFR_IA_Ordered = new ConflictFreeReplicatedIntAst(RId(1), new MockedTransmitter(network))
      val CFR_IA_Shuffled = new ConflictFreeReplicatedIntAst(RId(1), new MockedTransmitter(network))

      val operations = operations_subset.toSeq
      CFR_IA_Ordered merge operations
      // src: https://alvinalexander.com/source-code/scala-how-to-shuffle-list-randomize/
      CFR_IA_Shuffled merge rnd.shuffle(operations)

      assert(CFR_IA_Ordered.query isomorphic CFR_IA_Shuffled.query)
    }
  }

  "Position updates are correct" in {
    def chars_before_after(pos: Int, text: String) = Seq(text.take(pos).last, text.drop(pos).head)

    object EmptyTransmitter extends TX[ReplicatedIntOp] {
      override def publish(value: Seq[ReplicatedIntOp]): Unit = ()

      override def subscribe(callback: Seq[ReplicatedIntOp] => Unit): Unit = ()
    }

    val conflictFreeReplicatedIntAst = ConflictFreeReplicatedIntAst(RId(0), EmptyTransmitter)
    conflictFreeReplicatedIntAst.update(0, "(define foo bar)")
    val oldSource = "(define foo bar)"
    val oldPos = 10 //         `--> between the o's of 'FOO'
    assert(chars_before_after(oldPos, oldSource) == Seq('o', 'o'))

    val (newPos, newSource) = ConflictFreeReplicatedIntAst.passiveUpdate(oldPos, oldSource, conflictFreeReplicatedIntAst)
    assert(chars_before_after(oldPos, oldSource) == chars_before_after(newPos, newSource))
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val transmitter = new MockedTransmitter(mutable.Set())
    val conflictFreeReplicatedIntAst = ConflictFreeReplicatedIntAst(RId(0), transmitter)
    assert(ConflictFreeReplicatedIntAst.unapply(conflictFreeReplicatedIntAst).get == (RId(0), transmitter))
  }
}