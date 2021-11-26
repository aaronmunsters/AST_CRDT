package AST.Edit

import AST.HeadedAST

trait AstEdit[Identity] {
  def perform(ast: HeadedAST[Identity]): HeadedAST[Identity]
}
