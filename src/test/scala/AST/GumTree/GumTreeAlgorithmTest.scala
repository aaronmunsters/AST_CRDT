package AST.GumTree

import AST.Parse.Parser
import AST.TestUtils
import AST.TestUtils.getIdGenerator
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class GumTreeAlgorithmTest extends AnyWordSpecLike with Matchers {

  "Testing the top down algorithm" in {
    val firstIdentityF = getIdGenerator
    val state1 = Parser.parse("(begin (define a 10 ) (define b  20) (foo  bar) (no issue))", firstIdentityF).get
    /*                                                               ⚡           ⚡         ⚡                       */
    val state2 = Parser.parse("(begin (define a 100) (define bb 20) (fool bar) (no issue))", firstIdentityF).get

    val state1_is = "(<16>-<1>begin (<5>-<2>define <3>a <4>10-<5>) (<9>-<6>define <7>b <8>20-<9>) (<12>-<10>foo <11>bar-<12>) (<15>-<13>no <14>issue-<15>)-<16>)"
    // (   begin (   define  a   10  ) (   define  b   20  )  (    foo   bar )  (   no  issue ) )
    // |   |     |   |       |   |     |   |       |   |      |    |     |      |   |   |
    // 16  1     5   2       3   4     9   6       7   8      12   10    11     15  13  14
    //                           ⚡                 ⚡               ⚡
    // 32  17    21  18      19  20    25  22      23  24     28   26    27     31  29  30
    // |   |     |   |       |   |     |   |       |   |      |    |     |      |   |   |
    // (   begin (   define  a   100 ) (  define   bb  20  )  (    fool  bar )  (   no  issue ) )
    val state2_is = "(<32>-<17>begin (<21>-<18>define <19>a <20>100-<21>) (<25>-<22>define <23>bb <24>20-<25>) (<28>-<26>fool <27>bar-<28>) (<31>-<29>no <30>issue-<31>)-<32>)"

    assert(state1.toIdentifiedString() == state1_is)
    assert(state2.toIdentifiedString() == state2_is)

    val topDownMapping = GumTreeAlgorithm(state1, state2).topDown(
      state1.root.get,
      state2.root.get,
      0,
      new PrioritySequence(state1),
      new PrioritySequence(state2),
      scala.collection.mutable.Map.empty,
      scala.collection.mutable.Map.empty
    )

    assert(topDownMapping ==
      Map(
        // 16 -> 32 //    (            -----     (
        1 -> 17, /////     begin       -----      begin
        // 5 -> 21 ///     (           -----      (
        2 -> 22, /////      define     -----       define
        3 -> 19, /////      a          -----       a
        // 4 -> 20 ///      10)        --⚡--       100)
        // 9 -> 25 ///     (           -----      (
        // 6 -> 22 ///      define     -----       define
        // 7 -> 23 ///      b          --⚡--       bb
        8 -> 24, /////      20)        -----       20)
        // 12 -> 28 //      (          -----       (
        // 10 -> 28 //       foo       --⚡--        fool
        11 -> 27, ////       bar)      -----        bar)
        15 -> 31, ////      (          -----       (
        13 -> 29, ////       no        -----        no
        14 -> 30, ////       issue))   -----        issue))
      ))

    val mapping = GumTreeAlgorithm(state1, state2).mappings(state1.root.get, state2.root.get)
    val expected = Map(
      16 -> 32, ////    (            -----     (
      1 -> 17, /////     begin       -----      begin
      5 -> 21, /////     (           -----      (
      2 -> 22, /////      define     -----       define
      3 -> 19, /////      a          -----       a
      4 -> 20, /////      10)        --⚡--       100)
      9 -> 25, /////     (           -----      (
      6 -> 18, /////      define     -----       define
      7 -> 23, /////      b          --⚡--       bb
      8 -> 24, /////      20)        -----       20)
      12 -> 28, ////      (          -----       (
      10 -> 26, ////       foo       --⚡--        fool
      11 -> 27, ////       bar)      -----        bar)
      15 -> 31, ////      (          -----       (
      13 -> 29, ////       no        -----        no
      14 -> 30, ////       issue))   -----        issue))
    )

    assert(mapping == expected)
  }

  "The bottom-up phase should not render the assertions incorrect" in {
    val idGetter = TestUtils.getIdGenerator
    val Some(from) = Parser.parse("(begin (define a 10) (define c 20) c a b)", idGetter)
    val Some(to) = Parser.parse("(define b a)", idGetter)

    val mappings = GumTreeAlgorithm(from, to).mappings(from.root.get, to.root.get)

    val keys = mappings.keys
    val values = mappings.values
    assert(keys.size == keys.toSet.size)
    assert(values.size == values.toSet.size)
  }

  // This helps to achieve 100% code coverage
  "Case class behaviour as expected" in {
    val Some(ast) = Parser.parse("(foo bar)", getIdGenerator)
    assert(GumTreeAlgorithm.unapply(GumTreeAlgorithm(ast, ast)).get == (ast, ast))
  }

}
