package AST.Node

trait SchemeNode[Identity] {
  val id: Identity
  val parent: Option[Identity]
  def toString: String
}
