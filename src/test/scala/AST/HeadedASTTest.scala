package AST

import AST.Edit.Add
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber}
import utest._

object HeadedASTTest extends TestSuite {
  type Identity = Int
  var uniqueId: Identity = 0

  private def getIdentity = {
    uniqueId += 1
    uniqueId
  }

  override def tests: Tests = Tests {
    test("Creation") {
      val ast = HeadedAST.empty[Identity]
      assert(true)

      val plusWrapExpression = SchemeExpression(0, 0, getIdentity, None, Seq())
      val plusIdentifier = SchemeIdentifier(0, 0, getIdentity, Some(plusWrapExpression.id), "+")
      val numberLeft = SchemeNumber(0, 0, getIdentity, Some(plusWrapExpression.id), 1)
      val numberRight = SchemeNumber(0, 0, getIdentity, Some(plusWrapExpression.id), 2)

      val astExpectedString = "(+ 1 2)"
      val astWithSimpleAddition = ast
        .perform(Add.from(plusWrapExpression))
        .perform(Add.from(plusIdentifier))
        .perform(Add.from(numberLeft))
        .perform(Add.from(numberRight))

      assert(astExpectedString == astWithSimpleAddition.toAstString())

      assert(astWithSimpleAddition.perform(AST.Edit.AstEdit.Delete(plusIdentifier.id)).toAstString() == "(1 2)")

      val emptyAst = astWithSimpleAddition.perform(AST.Edit.AstEdit.Delete(plusWrapExpression.id))
      assert(emptyAst.toAstString() == "")
    }

    var id = 0
    val getId = () => {
      id += 1
      id
    }

    test("Positioning in the AST right after parsing") {
      def assert_position_character(source: String, position: Int, char: Char): Unit = {
        val tree = Parse.Parser.parseSchemeSmall(source, getId).get
        val (identity, offset) = tree.idAt(position).get
        assert(tree(identity).toAstString(tree)(offset) == char)
      }

      assert_position_character("(define abracadabra 91919191)", 13, 'a')
      assert_position_character("(define (abr aca dab ra) 91919191)", 14, 'c')
      assert_position_character("""(foo
                                  |     baz
                                  |     bar)""".stripMargin, 11, 'a')

      {
        // Tests for the cursor in the middle of (), testing that it remains at the correct possition
        val source = "()"
        val tree: HeadedAST[Int] = Parse.Parser.parseSchemeSmall(source, getId).get
        val (identity, offset) = tree.idAt(1).get
        assert(tree.startPos(identity) + offset == 1)
      }
    }

    test("Positioning in the ast after reformatting") {
      {
        // Tests for the cursor touching the ending of the a
        val source = "(define a)"
        val tree: HeadedAST[Int] = Parse.Parser.parseSchemeSmall(source, getId).get
        val (identity, _) = tree.idAtConsidering(9, source, getId).get

        assert(tree(identity).isInstanceOf[SchemeIdentifier[_]])
        assert(tree(identity).asInstanceOf[SchemeIdentifier[_]].value == "a")
      }

      def assert_keeping_position(source: String, position: Int): Unit = {
        val tree = Parse.Parser.parseSchemeSmall(source, getId).get
        val (identity, offset) = tree.idAtConsidering(position, source, getId).get
        assert(tree.startPos(identity) + offset == position)
      }

      // Tests for the cursor right after the 'ab', testing whether siblings that come after are not added
      assert_keeping_position("""(define
                                |    ab
                                |    99)""".stripMargin, 14)
      // Tests for the cursor right after the 'eg', testing whether siblings that come after are not added (letters)
      assert_keeping_position("""(abcd
                                |    eg
                                |    f)""".stripMargin, 12)
      // Tests for the cursor right after the 'g', testing whether siblings with parentheses indent correctly
      assert_keeping_position("""(abcd
                                |    (a)
                                |    g)""".stripMargin, 19)

    }
  }
}
