package AST.Edit
import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}

object Delete {
  def apply[Identity](node: SchemeNode[Identity]): Delete[Identity] = Delete(node.id)
}

case class Delete[Identity](target: Identity) extends AstEdit[Identity] {
  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    if(ast hasRoot target) return HeadedAST.empty
    if(! (ast contains target)) return ast

    // Ast contains node and it is not the root
    val targetTree = ast.header(target)
    // 1. Remove children
    val withoutChildren = targetTree match {
      case schemeExpression: SchemeExpression[Identity] =>
        schemeExpression.children.foldLeft(ast)((ast, toRemove) => ast.perform(Delete(toRemove)))
      case _ => ast
    }
    // 2. Remove self
    ast.header(targetTree.parent.get) match {
      case expression: SchemeExpression[Identity] =>
        val updatedExpression = expression.removeChild(target)
        val updatedHeader = ast.header.updated(updatedExpression.id, updatedExpression).removed(target)
        ast.copy(header = updatedHeader)
      case _ => ast // TODO: throw error!
    }
  }
}
