package AST.Edit

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode}

case class Add[Identity](tree: SchemeNode[Identity], parent: Option[Identity], index: Int) extends AstEdit[Identity] {
  private def updateParent(ast: HeadedAST[Identity], parentTree: SchemeNode[Identity]): HeadedAST[Identity] = {
    parentTree match {
      case expression: SchemeExpression[Identity] =>

        val updatedExpression = expression.copy(subexpressions =
          if(index < expression.subexpressions.length)
            expression.subexpressions.updated(index, tree.id)
          else
            expression.subexpressions :+ tree.id
        )
        val updatedHeader = ast.header.updated(parentTree.id, updatedExpression).updated(tree.id, tree)
        ast.copy(header = updatedHeader)
      case _ => ast
    }
  }

  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    assert(!(ast contains tree.id))
    assert(tree.parent == parent)
    parent match {
      case None => HeadedAST.withRoot(tree)
      case Some(parentIdentity) => ast.header.get(parentIdentity) match {
        case Some(parentTree) =>  updateParent(ast, parentTree)
        case None => ast // Parent could not be found, thus leave AST unchanged
      }
    }
  }
}

object Add {
  def from[Identity](tree: SchemeNode[Identity]): Add[Identity] = {
    Add(tree, tree.parent, Int.MaxValue)
  }
}