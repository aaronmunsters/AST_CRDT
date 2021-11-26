package AST

import AST.Edit.AstEdit
import AST.Node.SchemeNode

case class ConflictFreeReplicatedAst[Identity](root: SchemeNode[Identity]) {
  def update(operation: AstEdit[Identity]): ConflictFreeReplicatedAst[Identity] = {
    null
  }

  def query: SchemeNode[Identity] = root


  def merge(operation: AstEdit[Identity]): ConflictFreeReplicatedAst[Identity] = {
    null
  }
}
