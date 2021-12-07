package AST.Edit

import AST.Edit.AstEdit.Add
import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeNode}

object Add {
  def perform[Identity](ast: HeadedAST[Identity], add: Add[Identity]): HeadedAST[Identity] = add match {
    case AST.Edit.AstEdit.Add(tree, parent, index) => assert(!(ast contains tree.id))
      parent match {
        case None =>
          HeadedAST.withRoot(tree)
        case Some(parentIdentity) =>
          assert(ast contains parentIdentity)
          if (!(ast contains parentIdentity))
            ast
          else
            ast.header(parentIdentity) match {
              case expression: SchemeExpression[Identity] =>
                val updatedExpression = expression.addChild(tree.id, index)
                val updatedHeader = ast.header.updated(ast.header(parentIdentity).id, updatedExpression).updated(tree.id, tree)
                ast.copy(header = updatedHeader)
              case _ => ast
            }
      }
  }

  def from[Identity](tree: SchemeNode[Identity]): Add[Identity] = {
    AST.Edit.AstEdit.Add(tree, tree.parent, Int.MaxValue)
  }
}
