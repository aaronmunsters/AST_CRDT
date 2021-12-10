package AST.Parse

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}
import fastparse._
import MultiLineWhitespace._ // ignore whitespaces, newlines etc.

object Parser {
  def parseSchemeSmall[Identity](source: String, getIdentity: () => Identity): Option[HeadedAST[Identity]] = {

    def identifier[_: P] =
      P(Index ~ CharsWhileIn("a-zA-Z_=!@#$%^&*=+'\\/<>").! ~ Index)
        .map { case (start, identifier, end) => HeadedAST.withRoot(SchemeIdentifier(start, end, getIdentity(), None, identifier))}

    def string[_: P] =
      P(Index ~ "\"" ~ CharsWhile(_ != '\"').rep.! ~ "\"" ~ Index)
        .map { case (start, string, end) => HeadedAST.withRoot(SchemeString(start, end, getIdentity(), None, string)) }

    def number[_: P] = P(Index ~ CharsWhileIn("0-9").! ~ Index)
      .map { case (start, number, end) => HeadedAST.withRoot(SchemeNumber(start, end, getIdentity(), None, number.toInt)) }

    def term[_: P] = P(number | string | identifier)

    def expression[_: P]: P[HeadedAST[Identity]] =
      P(Index ~ "(" ~/ (term | expression).rep ~ ")" ~ Index)
        .map { case (start, childrenHeadedAsts, end) => childrenHeadedAsts
          .foldRight(
            HeadedAST.withRoot(SchemeExpression(start, end, getIdentity(), None, Seq()))
          )((childHeadedAst, soFar) => {
            val updatedChild = childHeadedAst(childHeadedAst.root.get).withParent(soFar.header(soFar.root.get).id)
            val updatedExpression = soFar.header(soFar.root.get).asInstanceOf[SchemeExpression[Identity]].prependChild(updatedChild.id)
            val updatedHeader: Map[Identity, SchemeNode[Identity]] = (soFar.header ++ childHeadedAst.header)
              .updated(updatedExpression.id, updatedExpression)
              .updated(updatedChild.id, updatedChild)
            soFar.copy(header = updatedHeader)
          }) }

    def program[_: P] = P(Start ~ expression ~ End)

    parse(source, program(_)) match {
      case Parsed.Success(headedAST: HeadedAST[Identity], index) if index == source.length => Some(headedAST)
      case _ => None
    }
  }
}
