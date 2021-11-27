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
    println(Parser.parseSchemeSmall(goalSourcecode, getIdentity).get.toPrettyAstString())


    println(Parser.parseSchemeSmall("((((((     thisIsAnIdentifier    ))))))", getIdentity).get.toPrettyAstString())
    println(Parser.parseSchemeSmall("(    100394   10   )", getIdentity).get.toPrettyAstString())
    println(Parser.parseSchemeSmall("(\"foo      Bar\")", getIdentity).get.toPrettyAstString())
  }
}
