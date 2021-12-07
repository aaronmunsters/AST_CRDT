package AST.Edit

import AST.Edit.AstEdit.Move
import AST.HeadedAST
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber, SchemeString}

object Move {

  def perform[Identity](ast: HeadedAST[Identity], move: Move[Identity]): HeadedAST[Identity] = move match {
    case AST.Edit.AstEdit.Move(child, newParent, index) =>
      // Preconditions:
      // 1) both nodes are in the ast
      assert((ast contains child) && (ast contains newParent))
      // 2) the child is not the root
      assert(!(ast hasRoot child))
      // 3) the child is no ancestor of the new parent
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
        case _: SchemeExpression[Identity] if moveInSameParent => updatedNewParent
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
