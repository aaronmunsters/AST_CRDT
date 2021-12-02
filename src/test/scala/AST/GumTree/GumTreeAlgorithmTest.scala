package AST.GumTree

import AST.Parse.Parser
import utest.{TestSuite, Tests, test}

object GumTreeAlgorithmTest extends TestSuite {
  type Identity = Int

  private def getIdentityF = {
    var identity: Identity = 0
    () => {
      identity += 1;
      identity
    }
  }


  override def tests: Tests = Tests {
    test("Testing the top down algorithm") {
      val firstIdentityF = getIdentityF
      val state1 = Parser.parseSchemeSmall("(begin (define a 10 ) (define b  20) (foo  bar) (no issue))", firstIdentityF).get
      /*                                                               ⚡           ⚡         ⚡                       */
      val state2 = Parser.parseSchemeSmall("(begin (define a 100) (define bb 20) (fool bar) (no issue))", firstIdentityF).get

      // (   begin (   define  a   10  ) (   define  b   20  )  (    foo   bar )  (   no  issue ) )
      //
      // |   |     |   |       |   |     |   |       |   |      |    |     |      |   |   |
      // 0   1     2   3       4   5     6   7       8   9      10   11    12     13  14  15
      // 16  17    18  19      20  21    22  23      24  25     26   27    28     29  30  31
      // |   |     |   |       |   |     |   |       |   |      |    |     |      |   |   |
      //
      // (   begin (   define  a   100 ) (  define   bb  20  )  (    fool  bar )  (   no  issue ) )
      //                           ⚡                 ⚡               ⚡

      println(state1.toIdentifiedString())

      println(state2.toIdentifiedString())


      val topDownMapping = GumTreeAlgorithm(state1, state2).topDown(
        state1.root.get,
        state2.root.get,
        0,
        new PrioritySequence[Identity](state1),
        new PrioritySequence[Identity](state2),
        scala.collection.mutable.Map.empty[Identity, Identity],
        scala.collection.mutable.Map.empty[Identity, Identity]
      )

      topDownMapping.foreach { case (identity3, identity4) => println(s"$identity3: ${state1(identity3).toAstString(state1)} <-> $identity4: ${state2(identity4).toAstString(state2)}") }



      val mapping = GumTreeAlgorithm(state1, state2).mappings(state1.root.get, state2.root.get)
      mapping.foreach { case (identity3, identity4) => println(s"$identity3: ${state1(identity3).toAstString(state1)} <-> $identity4: ${state2(identity4).toAstString(state2)}") }

//      mapping.foreach { case (identity3, identity4) => println(s"$identity3: ${state1(identity3).toAstString(state1)} <-> $identity4: ${state2(identity4).toAstString(state2)}") }
//      println(mapping)


//      assert(GumTreeAlgorithm(state1, state2).mappings(state1.root.get, state2.root.get).map { case (from, to) => (state1(from).id, state2(to).id) } ==
//        Map(
//          // 0 -> 13 /////   (            -----     (
//          1 -> 14, ///////    begin       -----      begin
//          2 -> 15, ///////    (           -----      (
//          3 -> 16, ///////     define     -----       define
//          4 -> 17, ///////     a          -----       a
//          5 -> 18, ///////     10)        --⚡--       100)
//          6 -> 19, ///////    (           -----      (
//          7 -> 20, ///////     define     -----       define
//          8 -> 21, ///////     b          --⚡--       bb
//          9 -> 22, ///////     20)        -----       20)
//          10 -> 23, //////     (          -----       (
//          11 -> 24, //////      foo       --⚡--        fool
//          12 -> 25, //////      bar))     -----        bar))
//        ))
//
//
//      val secondIdentityF = getIdentityF
//      val state3 = Parser.parseSchemeSmall("  (begin (define a 10 ) (define b  20) (foo  bar))  ", getIdentityF).get
//      /*                                            ⚡⚡                                                                   */
//      val state4 = Parser.parseSchemeSmall("(((begin (define a 10 ) (define b  20) (foo  bar))))", getIdentityF).get
//
//      val mappings = GumTreeAlgorithm(state3, state4).mappings(state3.root.get, state4.root.get).map { case (from, to) => (state3(from).id, state4(to).id) }
//      mappings.foreach { case (identity3, identity4) => println(s"$identity3: ${state3(identity3).toAstString(state3)} <-> $identity4: ${state4(identity4).toAstString(state4)}") }
//
//      assert(GumTreeAlgorithm(state3, state4).mappings(state3.root.get, state4.root.get).map { case (from, to) => (state3(from).id, state4(to).id) } ==
//        Map(
//          1 -> 14, //   ---
//          2 -> 15, //   ---
//          3 -> 16, //   ---
//          4 -> 17, //   ---
//          6 -> 19, //   ---
//          7 -> 20, //   ---
//          8 -> 21, //   ---
//          10 -> 23, //  ---
//          11 -> 24, //  ---
//        ))
    }
  }


}
