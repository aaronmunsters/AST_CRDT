package AST.Node

object SchemeExpression {
  def empty[Identity](identity: Identity): SchemeExpression[Identity] = SchemeExpression(identity, None, Seq())
}

case class SchemeExpression[Identity](id: Identity,
                                      parent: Option[Identity],
                                      subexpressions: Seq[Identity]) extends RecursiveNode[Identity] {
  def contains(identity: Identity): Boolean = subexpressions.contains(identity)

  def prependChild(identity: Identity): SchemeExpression[Identity] = {
    copy(subexpressions = identity +: subexpressions)
  }

  // TODO: addChild for a given index will actually replace the node, needs to be fixed!
  def addChild(identity: Identity, index: Int): SchemeExpression[Identity] = {
    copy(subexpressions =
      if(index < subexpressions.length)
        subexpressions.updated(index, identity)
      else
        subexpressions :+ identity
    )
  }

  def removeChild(identity: Identity): SchemeExpression[Identity] =
    copy(subexpressions = subexpressions.filterNot(_ == identity))
}