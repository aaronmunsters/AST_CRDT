package AST.Parse

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}
import fastparse._
import MultiLineWhitespace._ // ignore whitespaces, newlines etc.

object Parser {
  def parseSchemeSmall[Identity](source: String, getIdentity: () => Identity): Option[HeadedAST[Identity]] = {

    def identifier[_: P] =
      P(CharsWhileIn("a-zA-Z_=!@#$%^&*=+'\\/<>").!)
        .map(identifier => HeadedAST.withRoot(SchemeIdentifier(getIdentity(), None, identifier)))

    def string[_: P] =
      P("\"" ~ CharsWhile(_ != '\"').rep.! ~ "\"")
        .map(string => HeadedAST.withRoot(SchemeString(getIdentity(), None, string)))

    def number[_: P] = P(("-".? ~ CharsWhileIn("0-9")).!.map(
      number => HeadedAST.withRoot(SchemeNumber(getIdentity(), None, number.toInt))))

    def term[_: P] = P(number | string | identifier)

    def expression[_: P]: P[HeadedAST[Identity]] =
      P("(" ~/ (term | expression).rep(1) ~ ")")
        .map(childrenHeadedAsts => childrenHeadedAsts
          .foldRight(
            HeadedAST.withRoot(SchemeExpression(getIdentity(), None, Seq()))
          )((childHeadedAst, soFar) => {
            val updatedExpression = soFar.header(soFar.root.get).asInstanceOf[SchemeExpression[Identity]].prependChild(childHeadedAst.root.get)
            val updatedHeader: Map[Identity, SchemeNode[Identity]] = (soFar.header ++ childHeadedAst.header).updated(updatedExpression.id, updatedExpression)
            soFar.copy(header = updatedHeader)
          }))

    def program[_: P] = P(Start ~ expression ~ End)

    parse(source, program(_)) match {
      case Parsed.Success(headedAST: HeadedAST[Identity], index) if index == source.length => Some(headedAST)
      case _ => None
    }
  }
}
