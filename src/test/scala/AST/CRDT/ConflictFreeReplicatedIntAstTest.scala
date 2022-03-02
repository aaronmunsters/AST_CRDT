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

  private class MockedDelayableTransmitter(network: scala.collection.mutable.Set[MockedDelayableTransmitter]) extends TX[ReplicatedIntOp] {
    private var activelyTransmitting = true
    private val operationBuffer = new mutable.Queue[Seq[ReplicatedIntOp]]()

    def buffer(): Unit = if (activelyTransmitting) activelyTransmitting = false

    def active(): Unit = if (!activelyTransmitting) {
      activelyTransmitting = true
      operationBuffer.dequeueAll(_ => true).foreach(publish)
    }

    var callback: Option[Seq[ReplicatedIntOp] => Unit] = None
    // add transmitter to network
    network.add(this)

    override def publish(value: Seq[ReplicatedIntOp]): Unit =
      if (activelyTransmitting) network.filter(_ != this).foreach(_.receive(value))
      else operationBuffer.enqueue(value)

    override def subscribe(callback: Seq[ReplicatedIntOp] => Unit): Unit =
      this.callback = Some(callback)

    def receive(operations: Seq[ReplicatedIntOp]): Unit = {
      assert(callback.nonEmpty)
      callback.foreach(_ (operations))
    }
  }

  "Test the replicated ASTs paper case" in {
    val network: mutable.Set[MockedDelayableTransmitter] = mutable.Set.empty
    val CFR_IA_1___Transmitter = new MockedDelayableTransmitter(network)
    val CFR_IA____2Transmitter = new MockedDelayableTransmitter(network)
    val CFR_IA_1___ = ConflictFreeReplicatedIntAst(RId(1), CFR_IA_1___Transmitter)
    val CFR_IA____2 = ConflictFreeReplicatedIntAst(RId(2), CFR_IA____2Transmitter)

    def assert_equality(): Unit = {
      assert(CFR_IA_1___.query isomorphic CFR_IA____2.query)
      assert(CFR_IA_1___.operations == CFR_IA____2.operations)
    }

    CFR_IA_1___.update(0, "(begin)")
    CFR_IA____2.update(0, "(begin ())")
    CFR_IA_1___.update(0, "(begin (eat))")
    CFR_IA____2.update(0, "(begin (eat \"apple\"))")
    CFR_IA_1___.update(0, "(begin (eat \"apple\") (touch))")
    CFR_IA____2.update(0, "(begin (eat \"apple\") (touch \"nose\"))")

    assert_equality()
    assert(CFR_IA_1___.query.toAstString() == "(begin (eat \"apple\") (touch \"nose\"))")
    assert(CFR_IA_1___.query.toAstString() == CFR_IA____2.query.toAstString())

    CFR_IA_1___Transmitter.buffer() // PAUSE THE NETWORK
    CFR_IA____2Transmitter.buffer() // PAUSE THE NETWORK

    // replica 1: change "apple" into "banana"
    CFR_IA_1___.update(0, "(begin (eat \"banana\") (touch \"nose\"))")
    // replica 2: swap the two statements
    CFR_IA____2.update(0, "(begin (touch \"nose\") (eat \"apple\"))")

    assert(CFR_IA_1___.query.toAstString() == "(begin (eat \"banana\") (touch \"nose\"))")
    assert(CFR_IA____2.query.toAstString() == "(begin (touch \"nose\") (eat \"apple\"))")

    CFR_IA_1___Transmitter.active() // REACTIVATE THE NETWORK
    CFR_IA____2Transmitter.active() // REACTIVATE THE NETWORK

    assert_equality()
    assert(CFR_IA_1___.query.toAstString() == "(begin (touch \"nose\") (eat \"banana\"))")
    assert(CFR_IA_1___.query.toAstString() == CFR_IA____2.query.toAstString())
  }

  "Test no cycles are introduced in the resulting AST" in {
    val network: mutable.Set[MockedDelayableTransmitter] = mutable.Set.empty
    val CFR_IA_1___Transmitter = new MockedDelayableTransmitter(network)
    val CFR_IA____2Transmitter = new MockedDelayableTransmitter(network)
    val CFR_IA_1___ = ConflictFreeReplicatedIntAst(RId(1), CFR_IA_1___Transmitter)
    val CFR_IA____2 = ConflictFreeReplicatedIntAst(RId(2), CFR_IA____2Transmitter)

    def assert_equality(): Unit = {
      assert(CFR_IA_1___.query isomorphic CFR_IA____2.query)
      assert(CFR_IA_1___.operations == CFR_IA____2.operations)
    }

    CFR_IA_1___.update(0, "((b) (c))")

    assert_equality()
    assert(CFR_IA_1___.query.toAstString() == "((b) (c))")
    assert(CFR_IA_1___.query.toAstString() == CFR_IA____2.query.toAstString())

    CFR_IA_1___Transmitter.buffer() // PAUSE THE NETWORK
    CFR_IA____2Transmitter.buffer() // PAUSE THE NETWORK

    // replica 1: make c child of b
    CFR_IA_1___.update(0, "((b (c)))")
    // replica 2: make b child of c
    CFR_IA____2.update(0, "((c (b)))")

    assert(CFR_IA_1___.query.toAstString() == "((b (c)))")
    assert(CFR_IA____2.query.toAstString() == "((c (b)))")

    CFR_IA_1___Transmitter.active() // REACTIVATE THE NETWORK
    CFR_IA____2Transmitter.active() // REACTIVATE THE NETWORK

    println(CFR_IA_1___.operations)

    {
      import AST.Edit.AstEdit.Add
      import AST.Edit.AstEdit.Move
      import AST.Node.SchemeNode.SchemeExpression
      import AST.Node.SchemeNode.SchemeIdentifier
      import AST.CRDT.ReplicatedIntOp.NId
      import AST.CRDT.ReplicatedIntOp.LClock

      val operations = List(
        /* CFR_IA_1___ adds "((b) (c))" */
        ReplicatedIntOp(
          (LClock(1), RId(1)),
          Add(
            SchemeExpression(1, 5, (RId(1), NId(2)), Some((RId(0), NId(0))), List()),
            Some((RId(0), NId(0))), 0)),
        ReplicatedIntOp(
          (LClock(2), RId(1)),
          Add(SchemeExpression(5, 8, (RId(1), NId(4)), Some((RId(0), NId(0))), List()), Some((RId(0), NId(0))), 1)),
        ReplicatedIntOp(
          (LClock(3), RId(1)),
          Add(SchemeIdentifier(2, 3, (RId(1), NId(1)), Some((RId(1), NId(2))), "b"), Some((RId(1), NId(2))), 0)),
        ReplicatedIntOp(
          (LClock(4), RId(1)),
          Add(SchemeIdentifier(6, 7, (RId(1), NId(3)), Some((RId(1), NId(4))), "c"), Some((RId(1), NId(4))), 0)),

        /* CFR_IA_1___ makes `c` a child of `b` */
        ReplicatedIntOp(
          (LClock(5), RId(1)),
          Move((RId(1), NId(4)), (RId(1), NId(2)), 1)),

        /* CFR_IA____2 makes `c` a child of `b` */
        ReplicatedIntOp(
          (LClock(5), RId(2)),
          Move((RId(1), NId(2)), (RId(1), NId(4)), 1)))

      assert(CFR_IA_1___.operations == operations)
    }

    assert_equality()
    println(CFR_IA_1___.query.toAstString())
    assert(CFR_IA_1___.query.toAstString() == "((b (c)))")
    assert(CFR_IA_1___.query.toAstString() == CFR_IA____2.query.toAstString())
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
