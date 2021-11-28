package GumTree.InMemoryAst.Node

case class SchemeString[Identity](id: Identity,
                                  content: String,
                                  subNodes: Seq[SchemeNode[Identity]]) extends SchemeNode[Identity] {
  def height: Int = 1

  def isomorphic(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeString: SchemeString[Identity] => schemeString.content == content
      case _ => false
    }
  }

  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeString: SchemeString[Identity] => true
      case _ => false
    }
  }

  def toIdentifiedString: String = s"<$id>$content"
}
