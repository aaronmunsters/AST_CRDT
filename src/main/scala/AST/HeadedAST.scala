package AST

import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}
import AST.Parse.Parser

object HeadedAST {
  def empty[Identity]: HeadedAST[Identity] = HeadedAST(Map(), None)
  def withRoot[Identity](root: SchemeNode[Identity]): HeadedAST[Identity] =
    HeadedAST(Map(root.id -> root), Some(root.id))
}

case class HeadedAST[Identity](header: Map[Identity, SchemeNode[Identity]], root: Option[Identity]) {
  def perform(edit: Edit.AstEdit[Identity]): HeadedAST[Identity] = edit.perform(this)

  def contains(identity: Identity): Boolean = header.contains(identity)

  def hasRoot(identity: Identity): Boolean = root contains identity

  def isAncestorOf(potentialParent: Identity, potentialChild: Identity): Boolean = {
    assert(contains(potentialParent) && contains(potentialChild))
    val parentIdOption = header(potentialChild).parent
    if (parentIdOption.isEmpty) return false
    val parentId = parentIdOption.get
    if (parentId == potentialParent) return true
    isAncestorOf(potentialParent, parentId)
  }

  def toAstString(start: Option[Identity] = root): String = {
    def nodeToString(nodeIdentity: Identity): String = {
      header.get(nodeIdentity) match {
        case Some(node) => node match {
          case expression: SchemeExpression[Identity] => s"(${expression.subexpressions.map(nodeToString).mkString(" ")})"
          case identifier: SchemeIdentifier[Identity] => identifier.identifier
          case number: SchemeNumber[Identity] => number.number.toString
          case string: SchemeString[Identity] => '"' + string.content + '"'
        }
        case None => "" // TODO: Should throw error?
      }
    }

    start match {
      case Some(startNode) => nodeToString(startNode)
      case None => ""
    }
  }

  def toPrettyAstString(indentation: Int = 4): String = {
    def nodeToString(depth: Int, nodeIdentity: Identity): String = {
      val subtree = header.get(nodeIdentity) match {
        case Some(node) => node match {
          case expression: SchemeExpression[Identity] =>
              expression.subexpressions.headOption match {
              case None => "()\n"
              case Some(head) =>
                val headString = nodeToString(0, head)
                val tailString = expression.subexpressions.tail.map(nodeToString(depth+indentation, _))
                s"(${(Seq(headString) ++ tailString).mkString("\n")})"
            }
          case identifier: SchemeIdentifier[Identity] => identifier.identifier
          case number: SchemeNumber[Identity] => number.number.toString
          case string: SchemeString[Identity] => '"' + string.content + '"'
        }
        case None => "" // TODO: Should throw error?
      }
      " " * depth ++ subtree
    }

    root match {
      case Some(rootNode) => nodeToString(0, rootNode)
      case None => ""
    }
  }
}
