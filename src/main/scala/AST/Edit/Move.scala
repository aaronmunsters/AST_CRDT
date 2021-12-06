package AST.Edit

import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}

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

    val childTree = ast.header(child)
    val newParentTree = ast.header(newParent)
    val oldParentTree = ast.header(childTree.parent.get)

    val moveInSameParent = newParentTree == oldParentTree

    val updatedNewParent = newParentTree match {
      case expression: SchemeExpression[Identity] =>
        val (before, after) = expression.children.filterNot(_ == child).splitAt(index)
        expression.copy(children = before ++ Seq(child) ++ after)
      case old => old
    }

    val updatedOldParent = oldParentTree match {
      case expression: SchemeExpression[Identity] if !moveInSameParent => expression.removeChild(child)
      case expression: SchemeExpression[Identity] if moveInSameParent => updatedNewParent
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
