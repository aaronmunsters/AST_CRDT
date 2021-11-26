package AST.Edit
import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}

case class Delete[Identity](target: Identity) extends AstEdit[Identity] {
  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    ast.header.get(target) match {
      case Some(targetTree) => targetTree.parent match {
        case Some(parent) => ast.header.get(parent) match {
          case Some(parentTree) => parentTree match {
            case expression: SchemeExpression[Identity] =>
              val updatedExpression = expression.copy(subexpressions = expression.subexpressions.filterNot(_ == target))
              val updatedHeader = ast.header.updated(updatedExpression.id, updatedExpression).removed(target)
              ast.copy(header = updatedHeader)
            case _ => ast // target deletion node has no parent
          }
          case None => // target node parent invalid, TODO: check tree as this is an invalid state
            ast
        }
        case None => HeadedAST(Map(), None)
      }
      case None => ast
    }
  }
}
