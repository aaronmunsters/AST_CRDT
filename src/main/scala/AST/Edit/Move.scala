package AST.Edit

import AST.Edit.AstEdit.Move
import AST.HeadedAST
import AST.Node.SchemeNode.RecursiveNode

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

      assert(newParentTree.isInstanceOf[RecursiveNode[Identity]])
      val (before, after) =
        newParentTree.asInstanceOf[RecursiveNode[Identity]].children.filterNot(_ == child).splitAt(index)
      val updatedNewParent =
        newParentTree.asInstanceOf[RecursiveNode[Identity]].withChildren(before ++ Seq(child) ++ after)

      val updatedOldParent =
        if (moveInSameParent) updatedNewParent
        else oldParentTree.asInstanceOf[RecursiveNode[Identity]].removeChild(child)

      val updatedChild = childTree.withParent(newParent)

      ast.copy(header = ast.header
        .updated(newParent, updatedNewParent)
        .updated(oldParentTree.id, updatedOldParent)
        .updated(child, updatedChild)
      )
  }
}
