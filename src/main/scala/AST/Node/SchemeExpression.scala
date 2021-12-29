package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.{RecursiveNode, SchemeExpression}

object SchemeExpression {
  def empty[Identity](start: Int, end: Int, identity: Identity): SchemeExpression[Identity] =
    AST.Node.SchemeNode.SchemeExpression(start, end, identity, None, Seq())
}
