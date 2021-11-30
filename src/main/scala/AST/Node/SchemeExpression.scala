package AST.Node

object SchemeExpression {
  def empty[Identity](identity: Identity): SchemeExpression[Identity] = SchemeExpression(identity, None, Seq())
}

case class SchemeExpression[Identity](id: Identity,
                                      parent: Option[Identity],
                                      children: Seq[Identity]) extends RecursiveNode[Identity] {
  def prependChild(identity: Identity): SchemeExpression[Identity] =
    copy(children = identity +: children)

  /**
   * Inserts the given identity at the position specified by index, causing the elements after it to move by 1
   *
   * @param identity : the identity to add
   * @param index    : the index in the sequence of children where to add the identity
   * @return a copy of the SchemeExpression with the newly added child
   */
  def addChild(identity: Identity, index: Int): SchemeExpression[Identity] =
    copy(children =
      if (index < children.length) {
        children.take(index) ++ Seq(identity) ++ children.drop(index)
      } else
        children :+ identity
    )

  def removeChild(identity: Identity): SchemeExpression[Identity] =
    copy(children = children.filterNot(_ == identity))

  override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
    case _: SchemeExpression[_] => true
    case _ => false
  }

  override def sameValue(n: SchemeNode[Identity]): Boolean = sameLabel(n)
}