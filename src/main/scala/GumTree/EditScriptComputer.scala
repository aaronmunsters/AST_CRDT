package GumTree

import AST.Edit.{AstEdit, UpdateValue}
import AST.HeadedAST
import GumTree.InMemoryAst.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}

object EditScriptComputer {
  def compute[Identity](before: HeadedAST[Identity], after: HeadedAST[Identity]): Option[Seq[AstEdit[Identity]]] = {
    (for {
      beforeInMem <- GumTree.InMemoryAst.Node.SchemeNode.from(before)
      afterInMem <- GumTree.InMemoryAst.Node.SchemeNode.from(after)
    } yield (beforeInMem, afterInMem)).map { case (beforeInMem, afterInMem) =>
      val mapping = GumTreeAlgorithm.mappings(beforeInMem, afterInMem)
      // For all mappings, check if the value has changed:
      mapping.map { case (before_node, after_node) =>
        assert(before_node sameLabel after_node)
        val newValue = after_node match {
          case _: SchemeExpression[Identity] => None
          case i: SchemeIdentifier[Identity] =>
            if (before_node.asInstanceOf[SchemeIdentifier[Identity]].identifier != i.identifier) Some(i.identifier) else None
          case n: SchemeNumber[Identity] =>
            if (before_node.asInstanceOf[SchemeNumber[Identity]].number != n.number) Some(n.number) else None
          case s: SchemeString[Identity] =>
            if (before_node.asInstanceOf[SchemeString[Identity]].content != s.content) Some(s.content) else None
        }
        newValue.map(UpdateValue(before_node.id, _))
      }.toSeq.flatten

      // TODO: add support for deletion, addition, wrapping and moves
    }
  }
}
