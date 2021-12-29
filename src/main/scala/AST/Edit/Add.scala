package AST.Edit

import AST.Edit.AstEdit.Add
import AST.HeadedAST
import AST.Node.SchemeNode
import AST.Node.SchemeNode.SchemeExpression

object Add {
  def perform[Identity](ast: HeadedAST[Identity], add: Add[Identity]): HeadedAST[Identity] = add match {
    case AST.Edit.AstEdit.Add(tree, None, _) => HeadedAST.withRoot(tree)
    case AST.Edit.AstEdit.Add(tree, Some(parentIdentity), index) =>
      assert(!(ast contains tree.id))
      assert(ast contains parentIdentity)
      ast.header(parentIdentity) match {
        case expression: SchemeExpression[Identity] =>
          val updatedExpression = expression.addChild(tree.id, index)
          val updatedHeader = ast.header.updated(ast.header(parentIdentity).id, updatedExpression).updated(tree.id, tree)
          ast.copy(header = updatedHeader)
      }
  }

  def from[Identity](tree: SchemeNode[Identity]): Add[Identity] = {
    AST.Edit.AstEdit.Add(tree, tree.parent, Int.MaxValue)
  }
}
