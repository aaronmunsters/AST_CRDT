package AST.Edit

import AST.Edit.AstEdit.{UpdateNumber, UpdateString}
import AST.HeadedAST
import AST.Node.SchemeNode.LeafNode

object UpdateValue {
  def apply[Identity](target: Identity, value: Any): AstEdit[Identity] =
    value match {
      case l: Long => UpdateNumber(target, l)
      case s: String => UpdateString(target, s)
    }

  def perform[Value, Identity](ast: HeadedAST[Identity], updateValue: UpdateNumber[Identity]): HeadedAST[Identity] = updateValue match {
    case AST.Edit.AstEdit.UpdateNumber(target, value) =>
      ast.header.get(target) match {
        case None => ast
        case Some(targetTree) =>
          ast.copy(header = ast.header.updated(target, targetTree.asInstanceOf[LeafNode[Identity, Long]].withValue(value)))
      }
  }

  def perform[Value, Identity](ast: HeadedAST[Identity], updateValue: UpdateString[Identity]): HeadedAST[Identity] = updateValue match {
    case AST.Edit.AstEdit.UpdateString(target, value) =>
      ast.header.get(target) match {
        case None => ast
        case Some(targetTree) =>
          ast.copy(header = ast.header.updated(target, targetTree.asInstanceOf[LeafNode[Identity, String]].withValue(value)))
      }
  }
}
