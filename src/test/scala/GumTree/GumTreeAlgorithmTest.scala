package GumTree

import AST.Parse.Parser
import GumTree.InMemoryAst.Node.SchemeNode
import GumTree.PrioritySequence.PrioritySequence
import utest.{TestSuite, Tests, test}

object GumTreeAlgorithmTest extends TestSuite {
  type Identity = Int
  private var identity: Identity = 0
  private val getIdentity = () => {
    identity += 1
    identity
  }

  def inMemTreeFrom(source: String): Option[GumTree.InMemoryAst.Node.SchemeNode[Identity]] = {
    val headedAst = Parser.parseSchemeSmall(source, getIdentity).get
    GumTree.InMemoryAst.Node.SchemeNode.from(headedAst)
  }

  override def tests: Tests = Tests {
    test("Testing the top down algorithm") {
      val state1 = inMemTreeFrom("(begin (define a 10 ) (define b  20) (foo  bar))").get
      /*                                                     ⚡           ⚡         ⚡          */
      val state2 = inMemTreeFrom("(begin (define a 100) (define bb 20) (fool bar))").get

      val mappings = GumTreeAlgorithm.topDown(
        state1,
        state2,
        1,
        new PrioritySequence[Identity](),
        new PrioritySequence[Identity](),
        scala.collection.mutable.Map.empty[SchemeNode[Identity], SchemeNode[Identity]],
        scala.collection.mutable.Map.empty[SchemeNode[Identity], SchemeNode[Identity]]
      )

      assert(state1.descendants.size + 1 == 13)

      // (  begin (  define  a  10  ) (  define  b   20  )  (    foo   bar ) )
      //
      // |  |     |  |       |  |     |  |       |   |      |    |     |
      // 1  2     3  4       5  6     7  8       9   10     11   12    13
      // |  |     |  |       |  |     |  |       |   |      |    |     |
      //
      // (  begin (  define  a  100 ) (  define  bb  20)    (    fool  bar ) )
      //                        ⚡                ⚡               ⚡

      println(state1.toIdentifiedString)
      println(state2.toIdentifiedString)
      println(GumTreeAlgorithm.mappings(state1, state2).map { case (from, to) => s"${from.id}->${to.id}"})

      val state3 = inMemTreeFrom("(begin (define a 10 ) (define b  20) (foo  bar))").get
      val state4 = inMemTreeFrom("(((begin (define a 10 ) (define b  20) (foo  bar))))").get

      println(state3.toIdentifiedString)
      println(state4.toIdentifiedString)
      println(GumTreeAlgorithm.mappings(state3, state4).map { case (from, to) => s"${from.id}->${to.id}"})
    }
  }
}
