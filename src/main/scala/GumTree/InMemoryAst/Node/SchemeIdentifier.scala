package GumTree.InMemoryAst.Node

case class SchemeIdentifier[Identity](id: Identity,
                                      identifier: String,
                                      descendants: Seq[SchemeNode[Identity]]) extends SchemeNode[Identity] {
  def height: Int = 1

  def isomorphic(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeIdentifier: SchemeIdentifier[Identity] => schemeIdentifier.identifier == identifier
      case _ => false
    }
  }

  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case _: SchemeIdentifier[Identity] => true
      case _ => false
    }
  }

  def sameNodeValue(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeIdentifier: SchemeIdentifier[Identity] => identifier == schemeIdentifier.identifier
      case _ => false
    }
  }

  def toIdentifiedString: String = s"<$id>$identifier"
}