package AST.CRDT

import AST.Edit.AstEdit

trait ReplicatedOperation[Identity, EditIdentity] {
  val editIdentity: EditIdentity
  val edit: AstEdit[Identity]
}
