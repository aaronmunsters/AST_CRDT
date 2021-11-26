package AST.Node

trait RecursiveNode[Identity] extends SchemeNode[Identity] {
  def contains(identity: Identity): Boolean
}
