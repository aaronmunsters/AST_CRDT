package GumTree

import AST.Edit.UpdateValue
import AST.Parse.Parser
import utest.{TestSuite, Tests, test}

object EditScriptComputerTest extends TestSuite {
  private var identity = 0
  private val getIdentity = () => {identity += 1; identity}

  private def edits(s1: String, s2: String) = {
    for {
      ast1 <- Parser.parseSchemeSmall(s1, getIdentity)
      ast2 <- Parser.parseSchemeSmall(s2, getIdentity)
      edits <- EditScriptComputer.compute(ast1, ast2)
    } yield edits
  }

  override def tests: Tests = Tests {
    test("Compute an edit script") {
      val edits_simple = edits(
        "(define a 10)",
        "(define b 20)"
      ).get

      assert(edits_simple.exists(e => e match {
        case UpdateValue(_, "b") => true
        case _ => false
      }))

      assert(edits_simple.exists(e => e match {
        case UpdateValue(_, 20) => true
        case _ => false
      }))

      assert(edits_simple.size == 2)
    }
  }
}
