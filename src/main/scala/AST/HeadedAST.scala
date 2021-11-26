package AST

import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}

object HeadedAST {
  def empty[Identity]: HeadedAST[Identity] = HeadedAST(Map(), None)
  def withRoot[Identity](root: SchemeNode[Identity]): HeadedAST[Identity] =
    HeadedAST(Map(root.id -> root), Some(root.id))
}

case class HeadedAST[Identity](header: Map[Identity, SchemeNode[Identity]], root: Option[Identity]) {
  def perform(edit: Edit.AstEdit[Identity]): HeadedAST[Identity] = edit.perform(this)

  def contains(identity: Identity): Boolean = header.contains(identity)

  def toAstString: String = {
    def nodeToString(nodeIdentity: Identity): String = {
      header.get(nodeIdentity) match {
        case Some(node) => node match {
          case expression: SchemeExpression[Identity] => s"(${expression.subexpressions.map(nodeToString).mkString(" ")})"
          case identifier: SchemeIdentifier[Identity] => identifier.identifier
          case number: SchemeNumber[Identity] => number.number.toString
          case string: SchemeString[Identity] => string.content
        }
        case None => "" // TODO: Should throw error?
      }
    }

    root match {
      case Some(rootNode) => nodeToString(rootNode)
      case None => ""
    }
  }
}
