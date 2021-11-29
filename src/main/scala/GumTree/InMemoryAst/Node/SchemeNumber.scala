package GumTree.InMemoryAst.Node

case class SchemeNumber[Identity](id: Identity,
                                  value: Long,
                                  descendants: Seq[SchemeNode[Identity]]) extends LeafNode[Identity, Long] {
  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case _: SchemeNumber[Identity] => true
      case _ => false
    }
  }

  def sameNodeValue(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeNumber: SchemeNumber[Identity] => schemeNumber.value == value
      case _ => false
    }
  }
}
