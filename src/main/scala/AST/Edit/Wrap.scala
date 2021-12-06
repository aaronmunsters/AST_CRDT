package AST.Edit
import AST.HeadedAST
import AST.Node.SchemeNode.RecursiveNode
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}

case class Wrap[Identity](child: Identity, parent: RecursiveNode[Identity]) extends AstEdit[Identity] {
  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    assert(parent contains child)
    ast.header.get(child) match {
      case None => ast
      case Some(childTree) =>

        // Child could be found, update its parent and add the parent to the header
        val updatedChild = childTree match {
          case expression: SchemeExpression[Identity] => expression.copy(parent = Some(parent.id))
          case identifier: SchemeIdentifier[Identity] => identifier.copy(parent = Some(parent.id))
          case number: SchemeNumber[Identity] => number.copy(parent = Some(parent.id))
          case string: SchemeString[Identity] => string.copy(parent = Some(parent.id))
        }
        val updatedHeader = ast.header.updated(parent.id, parent).updated(child, updatedChild)

        childTree.parent match {
          case None => assert(ast.root.get == child)
            HeadedAST(updatedHeader, Some(parent.id))
          case Some(oldParent) => ast.header.get(oldParent) match {
            case Some(oldParentTree) =>
              oldParentTree match {
                case SchemeExpression(id, parent, subexpressions) =>
                  val parentForWrapper = SchemeExpression(id, Some(oldParent), subexpressions.updated(subexpressions.indexOf(child), oldParent))
                  val updatedHeaderWithParent = updatedHeader.updated(parentForWrapper.id, parentForWrapper)
                  ast.copy(header = updatedHeaderWithParent)
                case _ => ast // TODO: this cannot happen?
              }
            case None => ast // TODO: check correctness of tree
          }
        }
    }
  }
}
