package AST.Edit

import AST.HeadedAST
import AST.Node.SchemeNode

sealed trait AstEdit[Identity] {
  def perform(ast: HeadedAST[Identity]): HeadedAST[Identity]
}

object AstEdit {
  trait Add[Identity] extends AstEdit[Identity] {
    val tree: SchemeNode[Identity]
    val parent: Option[Identity]
    val index: Int
  }

  trait Delete[Identity] extends AstEdit[Identity] {
    val target: Identity
  }

  trait Move[Identity] extends AstEdit[Identity] {
    val child: Identity
    val newParent: Identity
    val index: Int

  }

  trait UpdateValue[Identity] extends AstEdit[Identity] {
    val target: Identity
    val value: Any
  }
}
