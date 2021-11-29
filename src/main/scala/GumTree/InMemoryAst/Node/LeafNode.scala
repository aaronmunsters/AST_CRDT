package GumTree.InMemoryAst.Node

trait LeafNode[Identity, Content] extends SchemeNode[Identity] {
  def isomorphic(n: SchemeNode[Identity]): Boolean = sameNodeValue(n)
  def height: Int = 1
  val value: Content
  def toIdentifiedString: String = s"<$id>$value"
}
