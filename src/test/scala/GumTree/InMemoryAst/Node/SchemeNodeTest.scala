package GumTree.InMemoryAst.Node

import utest.{TestSuite, Tests, test}

object SchemeNodeTest extends TestSuite {
  override def tests: Tests = Tests {
    test("Generic methods") {
      assert(!(SchemeExpression(10, Seq(), Seq()) sameLabel SchemeIdentifier(10, "bar", Seq())))
    }
  }
}
