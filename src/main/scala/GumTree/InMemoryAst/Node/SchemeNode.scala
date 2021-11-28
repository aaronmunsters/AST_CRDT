package GumTree.InMemoryAst.Node

import AST.{HeadedAST, Node}

trait SchemeNode[Identity] {
  val id: Identity

  def toString: String

  def height: Int

  def isomorphic(n: SchemeNode[Identity]): Boolean

  def sameLabel(n: SchemeNode[Identity]): Boolean

  def sameNodeValue(n: SchemeNode[Identity]): Boolean

  val subNodes: Seq[SchemeNode[Identity]]

  def toIdentifiedString: String
}

object SchemeNode {
  def from[Identity](headedAST: HeadedAST[Identity]): Option[SchemeNode[Identity]] = {
    def crawl(identity: Identity): SchemeNode[Identity] = {
      headedAST.header(identity) match {
        case Node.SchemeExpression(id, _, subexpressions) =>
          val children = subexpressions.map(headedAST.header).map(_.id).map(crawl)
          val subNodes = children.flatMap(_.subNodes).reverse ++ children.reverse
          SchemeExpression(id, children, subNodes)
        case Node.SchemeIdentifier(id, _, identifier) =>
          SchemeIdentifier(id, identifier, Seq())
        case Node.SchemeNumber(id, _, number) =>
          SchemeNumber(id, number, Seq())
        case Node.SchemeString(id, _, content) =>
          SchemeString(id, content, Seq())
      }
    }
    headedAST.root.map(crawl)
  }

  def parentMap[Identity](schemeNode: SchemeNode[Identity]): Map[Identity, SchemeNode[Identity]] = {
    schemeNode match {
      case schemeExpression: SchemeExpression[Identity] =>
        schemeExpression.subexpressions.map(child => (child.id, schemeNode)).toMap ++
          schemeExpression.subexpressions.flatMap(parentMap[Identity])
      case _ => Map()
    }
  }
}