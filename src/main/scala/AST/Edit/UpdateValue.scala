package AST.Edit
import AST.Node.{SchemeIdentifier, SchemeNumber, SchemeString}
import AST.HeadedAST

case class UpdateValue[Value, Identity](target: Identity, value: Value) extends AstEdit[Identity] {
  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    ast.header.get(target) match {
      case None => ast
      case Some(targetTree) =>
        val updatedNode = value match {
          case string: String =>
            targetTree match {
              case identifierNode: SchemeIdentifier[Identity] => identifierNode.copy(identifier = string)
              case stringNode: SchemeString[Identity] => stringNode.copy(content = string)
              case _ => targetTree // do not update otherwise // TODO: replace with warning? Should not occur
            }
          case number: Long => {
            targetTree match {
              case numberNode: SchemeNumber[Identity] => numberNode.copy(number = number)
              case _ => targetTree // do not update otherwise // TODO: replace with warning? Should not occur
            }
          }
        }
        ast.copy(header = ast.header.updated(target, updatedNode))
    }
  }
}
