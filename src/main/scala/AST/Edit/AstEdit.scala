package AST.Edit

import AST.HeadedAST
import AST.Node.SchemeNode

sealed trait AstEdit[Identity] {
  def perform(ast: HeadedAST[Identity]): HeadedAST[Identity]
}

object AstEdit {
  case class Add[Identity](tree: SchemeNode[Identity], parent: Option[Identity], index: Int) extends AstEdit[Identity] {
    override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = AST.Edit.Add.perform(ast, this)
  }

  case class Delete[Identity](target: Identity) extends AstEdit[Identity] {
    override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = AST.Edit.Delete.perform(ast, this)
  }

  case class Move[Identity](child: Identity, newParent: Identity, index: Int) extends AstEdit[Identity] {
    def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] =
      AST.Edit.Move.perform(ast, this)
  }

  case class UpdateNumber[Identity](target: Identity, value: Long) extends AstEdit[Identity] {
    override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] =
      AST.Edit.UpdateValue.perform(ast, this)
  }

  case class UpdateString[Identity](target: Identity, value: String) extends AstEdit[Identity] {
    override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] =
      AST.Edit.UpdateValue.perform(ast, this)
  }
}
