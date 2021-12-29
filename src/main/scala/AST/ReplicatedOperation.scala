package AST

import AST.Edit._

trait ReplicatedOperation[Identity, EditIdentity] {
  val editIdentity: EditIdentity
  val edit: AstEdit[Identity]
}
