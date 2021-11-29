package GumTree.InMemoryAst.Node

case class SchemeString[Identity](id: Identity,
                                  value: String,
                                  descendants: Seq[SchemeNode[Identity]]) extends LeafNode[Identity, String] {
  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case _: SchemeString[Identity] => true
      case _ => false
    }
  }

  def sameNodeValue(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeString: SchemeString[Identity] => schemeString.value == value
      case _ => false
    }
  }
}
