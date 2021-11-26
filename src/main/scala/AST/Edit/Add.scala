package AST.Edit

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeNode}

case class Add[Identity](tree: SchemeNode[Identity], parent: Option[Identity], index: Int) extends AstEdit[Identity] {
  private def updateParent(ast: HeadedAST[Identity], parentTree: SchemeNode[Identity]): HeadedAST[Identity] = {
    parentTree match {
      case expression: SchemeExpression[Identity] =>
        val updatedExpression = expression.addChild(tree.id, index)
        val updatedHeader = ast.header.updated(parentTree.id, updatedExpression).updated(tree.id, tree)
        ast.copy(header = updatedHeader)
      case _ => ast
    }
  }

  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    assert(!(ast contains tree.id))
    assert(tree.parent == parent)
    parent match {
      case None =>
        HeadedAST.withRoot(tree)
      case Some(parentIdentity) =>
        if (ast contains parentIdentity) updateParent(ast, ast.header(parentIdentity)) else ast
    }
  }
}

object Add {
  def from[Identity](tree: SchemeNode[Identity]): Add[Identity] = {
    Add(tree, tree.parent, Int.MaxValue)
  }
}