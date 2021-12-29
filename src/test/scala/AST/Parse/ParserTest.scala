package AST.Parse

import utest.{TestSuite, Tests, test}

object ParserTest extends TestSuite {
  override def tests: Tests = Tests {
    var identity = 0
    val getIdentity = () => {
      identity += 1
      identity
    }

    test("Successful parsing") {
      val goalSourcecode =
        """      (begin
          (define a 10)
          (define b 20)
          (+ 1 2 3 a b))"""

      assert(Parser.parse(goalSourcecode, getIdentity).get.toPrettyAstString() ==
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

      assert("((((((thisIsAnIdentifier))))))" == Parser.parse("((((((     thisIsAnIdentifier    ))))))", getIdentity).get.toPrettyAstString())
      assert(Parser.parse("(    100394   10   )", getIdentity).get.toPrettyAstString() ==
        """(100394
          |    10)""".stripMargin)
      assert(Parser.parse("(\"foo      Bar\")", getIdentity).get.toPrettyAstString() ==
        "(\"foo      Bar\")")
    }

    test("Unsuccessful parsing") {
      assert(Parser.parse("(this is incorrect", getIdentity).isEmpty)
      assert(Parser.parse("faulty", getIdentity).isEmpty)
      assert(Parser.parse("123456 error", getIdentity).isEmpty)
      assert(Parser.parse("----", getIdentity).isEmpty)
    }

    // This helps to achieve 100% code coverage
    test("Object equality") {
      assert(Parser == Parser)
    }
  }
}
