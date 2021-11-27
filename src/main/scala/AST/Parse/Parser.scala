package AST.Parse

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}
import fastparse._
import NoWhitespace._ // ignore whitespaces, newlines etc.

// TODO: parser requires space before ending parenthesis, needs to be fixed!
object Parser {
  def parseSchemeSmall[Identity](source: String, getIdentity: () => Identity): Option[HeadedAST[Identity]] = {

    // === SPACES === //

    def space[_: P]: P[Unit] = P ( CharIn(" \t\r\n") )

    // === SPACES === //

    // === IDENTIFIER PARSING === //

    def identifier[_: P] =
      P((CharIn("a-zA-Z_-=!@#$%^&*=+'\\/<>") ~ CharsWhile(c => ! " \t\r\n".contains(c)).rep).!)
        .map(identifier => HeadedAST.withRoot(SchemeIdentifier(getIdentity(), None, identifier)))

    // === IDENTIFIER PARSING === //

    // === STRING PARSING === //

    def string[_: P] =
      P("\"" ~ CharsWhile(_ != '\"').rep.! ~ "\"")
        .map(string => HeadedAST.withRoot(SchemeString(getIdentity(), None, string)))

    // === STRING PARSING === //

    // === NUMBER PARSING === //

    def digits[_: P] = P(CharsWhileIn("0-9"))

    def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

    def fractional[_: P] = P("." ~ digits)

    def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

    def number[_: P] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      number => HeadedAST.withRoot(SchemeNumber(getIdentity(), None, number.toInt)))

    // === NUMBER PARSING === //

    // === TERM PARSING === //

    def term[_: P] = P(string | number | identifier)

    // === TERM PARSING === //

    // === EXPRESSION PARSING === //

    def expression[_: P]: P[HeadedAST[Identity]] =
      P("(" ~ space.rep ~ (expression | term).rep(sep = space.rep) ~ space.rep ~ ")")
        .map(childrenHeadedAsts => childrenHeadedAsts
          .foldRight(
            HeadedAST.withRoot(SchemeExpression(getIdentity(), None, Seq()))
          )((childHeadedAst, soFar) => {
            val updatedExpression = soFar.header(soFar.root.get).asInstanceOf[SchemeExpression[Identity]].prependChild(childHeadedAst.root.get)
            val updatedHeader: Map[Identity, SchemeNode[Identity]] = (soFar.header ++ childHeadedAst.header).updated(updatedExpression.id, updatedExpression)
            soFar.copy(header = updatedHeader)
          }))

    // === EXPRESSION PARSING === //

    def program[_: P] = P( space.rep ~ expression ~ space.rep ~ End)

    parse(source, program(_)) match {
      case Parsed.Success(headedAST: HeadedAST[Identity], index) => Some(headedAST)
      case _ => None
    }
  }
}
