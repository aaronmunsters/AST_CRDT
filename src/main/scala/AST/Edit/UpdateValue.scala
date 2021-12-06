package AST.Edit

import AST.HeadedAST
import AST.Node.SchemeNode.LeafNode

case class UpdateValue[Value, Identity](target: Identity, value: Value) extends AstEdit[Identity] {
  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    ast.header.get(target) match {
      case None => ast
      case Some(targetTree) =>
        ast.copy(header = ast.header.updated(target, targetTree.asInstanceOf[LeafNode[Identity, Value]].withValue(value)))
    }
  }
}
