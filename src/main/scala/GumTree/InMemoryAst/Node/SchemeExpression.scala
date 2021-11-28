package GumTree.InMemoryAst.Node

case class SchemeExpression[Identity](id: Identity,
                                      subexpressions: Seq[SchemeNode[Identity]],
                                      subNodes: Seq[SchemeNode[Identity]]) extends RecursiveNode[Identity] {
  def contains(identity: Identity): Boolean = subexpressions.exists(_.id == identity)

  def height: Int = subexpressions.map(_.height).max + 1

  def isomorphic(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeExpression: SchemeExpression[Identity] =>
        subexpressions.length == schemeExpression.subexpressions.length &&
          subexpressions.zip(schemeExpression.subexpressions).forall {
            case (myChild, otherChild) => myChild isomorphic otherChild
          }
      case _ => false
    }
  }

  def sameLabel(n: SchemeNode[Identity]): Boolean = {
    n match {
      case schemeExpression: SchemeExpression[Identity] => true
      case _ => false
    }
  }

  def toIdentifiedString: String = s"(<$id>-${subexpressions.map(_.toIdentifiedString).mkString(" ")}-<$id>)"
}