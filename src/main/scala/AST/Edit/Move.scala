package AST.Edit

import AST.HeadedAST
import AST.Node.{RecursiveNode, SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}

case class Move[Identity](child: Identity, newParent: Identity, index: Int) extends AstEdit[Identity] {

  // What needs to happen when moving tree A to tree B:
  //  - node A self: parent must be updated
  //  - node B self: list of children must be updated
  //  - Parent of A: list of children must be updated

  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    // Preconditions:
    // 1) both nodes are in the ast
    // 2) the child is not the root
    // 3) the child and parent have no ancestor relation
    assert(ast contains child)
    assert(ast contains newParent)
    assert(!(ast hasRoot child))
    assert(!ast.isAncestorOf(child, newParent))
    assert(!ast.isAncestorOf(newParent, child))

    val childTree = ast.header(child)
    val newParentTree = ast.header(newParent)
    val oldParentTree = ast.header(childTree.parent.get)

    val updatedNewParent = newParentTree match {
      case expression: SchemeExpression[Identity] => expression.addChild(child, index)
      case old => old
    }

    val updatedOldParent = oldParentTree match {
      case expression: SchemeExpression[Identity] => expression.removeChild(child)
      case old => old
    }

    val updatedChild = childTree match { // TODO: look for neater way to update this value
      case e: SchemeIdentifier[Identity] => e.copy(parent = Some(newParent))
      case e: SchemeNumber[Identity] => e.copy(parent = Some(newParent))
      case e: SchemeString[Identity] => e.copy(parent = Some(newParent))
      case e: SchemeExpression[Identity] => e.copy(parent = Some(newParent))
    }

    ast.copy(header = ast.header
      .updated(newParent, updatedNewParent)
      .updated(oldParentTree.id, updatedOldParent)
      .updated(child, updatedChild)
    )
  }
}
