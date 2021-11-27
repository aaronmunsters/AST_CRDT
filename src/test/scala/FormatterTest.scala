import utest.{TestSuite, Tests, test}

object FormatterTest extends TestSuite {
  override def tests: Tests = Tests {
    test("Formatting works") {
      val uglySource =
        """(begin     (   +
          |     (*
          |  1 2     3)
          |       4 5)
          |       "foobar")
          |""".stripMargin
      val niceSource =
        """(begin
          | (+
          |  (*
          |   1
          |   2
          |   3)
          |  4
          |  5)
          | "foobar")""".stripMargin

      assert(Formatter.format(uglySource, 1).get == niceSource)
      assert(Formatter.format("( (   test )   )", 1).get == "((test))")
    }
  }
}
