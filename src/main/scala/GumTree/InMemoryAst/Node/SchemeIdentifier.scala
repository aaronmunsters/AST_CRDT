package GumTree.InMemoryAst.Node

case class SchemeIdentifier[Identity](id: Identity,
                                      value: String,
                                      descendants: Seq[SchemeNode[Identity]]) extends LeafNode[Identity, String] {
  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case _: SchemeIdentifier[Identity] => true
      case _ => false
    }
  }

  def sameNodeValue(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeIdentifier: SchemeIdentifier[Identity] => value == schemeIdentifier.value
      case _ => false
    }
  }
}