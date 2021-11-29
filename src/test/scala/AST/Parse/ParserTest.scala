package AST.Parse

import utest.{TestSuite, Tests}

object ParserTest extends TestSuite {
  override def tests: Tests = Tests {
    var identity = 0
    val getIdentity = () => {
      identity += 1
      identity
    }

    val goalSourcecode =
      """      (begin
          (define a 10)
          (define b 20)
          (+ 1 2 3 a b))"""

    assert(Parser.parseSchemeSmall(goalSourcecode, getIdentity).get.toPrettyAstString() ==
      """(begin
        |    (define
        |        a
        |        10)
        |    (define
        |        b
        |        20)
        |    (+
        |        1
        |        2
        |        3
        |        a
        |        b))""".stripMargin
    )

    assert("((((((thisIsAnIdentifier))))))" == Parser.parseSchemeSmall("((((((     thisIsAnIdentifier    ))))))", getIdentity).get.toPrettyAstString())
    assert(Parser.parseSchemeSmall("(    100394   10   )", getIdentity).get.toPrettyAstString() ==
    """(100394
      |    10)""".stripMargin)
    assert(Parser.parseSchemeSmall("(\"foo      Bar\")", getIdentity).get.toPrettyAstString() ==
    "(\"foo      Bar\")")
  }
}
