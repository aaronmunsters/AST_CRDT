package GumTree.InMemoryAst.Node

case class SchemeNumber[Identity](id: Identity,
                                  number: Long,
                                  subNodes: Seq[SchemeNode[Identity]]) extends SchemeNode[Identity] {
  def height: Int = 1

  def isomorphic(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeNumber: SchemeNumber[Identity] => schemeNumber.number == number
      case _ => false
    }
  }

  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case _: SchemeNumber[Identity] => true
      case _ => false
    }
  }

  def sameNodeValue(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeNumber: SchemeNumber[Identity] => schemeNumber.number == number
      case _ => false
    }
  }

  def toIdentifiedString: String = s"<$id>$number"
}
