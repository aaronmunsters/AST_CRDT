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

      assert(GumTreeAlgorithm.mappings(state1, state2).map { case (from, to) => (from.id, to.id) } ==
        Map(12 -> 25, 4 -> 17, 8 -> 21, 9 -> 22, 6 -> 19, 1 -> 14, 3 -> 16, 10 -> 23, 5 -> 18, 11 -> 24, 2 -> 15, 7 -> 20))

      val state3 = inMemTreeFrom("  (begin (define a 10 ) (define b  20) (foo  bar))  ").get
      val state4 = inMemTreeFrom("(((begin (define a 10 ) (define b  20) (foo  bar))))").get

      assert(GumTreeAlgorithm.mappings(state3, state4).map { case (from, to) => (from.id, to.id) } ==
        Map(37 -> 50, 35 -> 48, 29 -> 42, 34 -> 47, 27 -> 40, 36 -> 49, 28 -> 41, 39 -> 52, 30 -> 43, 38 -> 51, 33 -> 46, 32 -> 45, 31 -> 44))
    }
  }
}
