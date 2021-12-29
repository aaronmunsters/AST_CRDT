package AST.Edit

import AST.Edit.AstEdit.Delete
import AST.HeadedAST
import AST.Node.SchemeNode
import AST.Node.SchemeNode.SchemeExpression

object Delete {
  def apply[Identity](node: SchemeNode[Identity]): Delete[Identity] = AST.Edit.AstEdit.Delete(node.id)

  def perform[Identity](ast: HeadedAST[Identity], delete: Delete[Identity]): HeadedAST[Identity] = delete match {
    case AST.Edit.AstEdit.Delete(target) if ast hasRoot target => HeadedAST.empty
    case AST.Edit.AstEdit.Delete(target) if !(ast contains target) => ast
    case AST.Edit.AstEdit.Delete(target) =>
      // Ast contains node and it is not the root
      val targetTree = ast.header(target)
      // 1. Remove children
      val withoutChildren = targetTree match {
        case schemeExpression: SchemeExpression[Identity] =>
          schemeExpression.children.foldLeft(ast)((ast, toRemove) => ast.perform(AST.Edit.AstEdit.Delete(toRemove)))
        case _ => ast
      }
      // 2. Remove self
      val parent = withoutChildren.header(targetTree.parent.get)
      assert(parent.isInstanceOf[SchemeExpression[Identity]])
      val updatedExpression = parent.asInstanceOf[SchemeExpression[Identity]].removeChild(target)
      val updatedHeader = ast.header.updated(updatedExpression.id, updatedExpression).removed(target)
      ast.copy(header = updatedHeader)
  }
}
