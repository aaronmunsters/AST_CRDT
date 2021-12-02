package AST

import AST.Node._

object HeadedAST {
  def empty[Identity]: HeadedAST[Identity] = HeadedAST(Map(), None)

  def withRoot[Identity](root: SchemeNode[Identity]): HeadedAST[Identity] =
    HeadedAST(Map(root.id -> root), Some(root.id))
}

case class HeadedAST[Identity](header: Map[Identity, SchemeNode[Identity]], root: Option[Identity]) {
  private implicit val headedAst: HeadedAST[Identity] = this

  def apply(identity: Identity): SchemeNode[Identity] = header(identity)

  def perform(edit: Edit.AstEdit[Identity]): HeadedAST[Identity] = edit.perform(this)

  def contains(identity: Identity): Boolean = header contains identity

  def hasRoot(identity: Identity): Boolean = root contains identity

  def isAncestorOf(potentialParent: Identity, potentialChild: Identity): Boolean = {
    assert(contains(potentialParent) && contains(potentialChild))
    val parentIdOption = header(potentialChild).parent
    if (parentIdOption.isEmpty) return false
    val parentId = parentIdOption.get
    if (parentId == potentialParent) return true
    isAncestorOf(potentialParent, parentId)
  }

  def toAstString(start: Option[Identity] = root): String =
    start.map(header(_).toAstString).getOrElse("")

  def toIdentifiedString(start: Option[Identity] = root): String =
    start.map(header(_).toIdentifiedString(this)).getOrElse("")

  def toPrettyAstString(indentation: Int = 4): String = {
    def nodeToString(depth: Int, nodeIdentity: Identity): String = {
      assert(contains(nodeIdentity))
      val subtree = header(nodeIdentity) match {
        case expression: SchemeExpression[Identity] =>
          if (expression.children.isEmpty) "()" else {
            val head :: tail = expression.children
            s"(${(nodeToString(0, head) +: tail.map(nodeToString(depth + indentation, _))).mkString("\n")})"
          }
        case node: LeafNode[_, _] => node.toAstString(this)
      }
      " " * depth ++ subtree
    }

    root.map(nodeToString(0, _)).getOrElse("")
  }
}
