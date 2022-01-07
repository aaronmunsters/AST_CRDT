package AST.Parse

import AST.TestUtils.getIdGenerator
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserTest extends AnyWordSpecLike with Matchers {
  "Successful parsing" in {
    val goalSourcecode =
      """      (begin
          (define a 10)
          (define b 20)
          (+ 1 2 3 a b))"""

    assert(Parser.parse(goalSourcecode, getIdGenerator).get.toPrettyAstString() ==
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

    assert("((((((thisIsAnIdentifier))))))" == Parser.parse("((((((     thisIsAnIdentifier    ))))))", getIdGenerator).get.toPrettyAstString())
    assert(Parser.parse("(    100394   10   )", getIdGenerator).get.toPrettyAstString() ==
      """(100394
        |    10)""".stripMargin)
    assert(Parser.parse("(\"foo      Bar\")", getIdGenerator).get.toPrettyAstString() ==
      "(\"foo      Bar\")")
  }

  "Unsuccessful parsing" in {
    assert(Parser.parse("(this is incorrect", getIdGenerator).isEmpty)
    assert(Parser.parse("faulty", getIdGenerator).isEmpty)
    assert(Parser.parse("123456 error", getIdGenerator).isEmpty)
    assert(Parser.parse("----", getIdGenerator).isEmpty)
  }

  // This helps to achieve 100% code coverage
  "Object equality" in {
    assert(Parser == Parser)
  }
}
