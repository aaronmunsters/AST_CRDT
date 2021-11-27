package AST.Node

object SchemeExpression {
  def empty[Identity](identity: Identity): SchemeExpression[Identity] = SchemeExpression(identity, None, Seq())
}

case class SchemeExpression[Identity](id: Identity,
                                      parent: Option[Identity],
                                      subexpressions: Seq[Identity]) extends RecursiveNode[Identity] {
  def contains(identity: Identity): Boolean = subexpressions.contains(identity)

  def prependChild(identity: Identity): SchemeExpression[Identity] =
    copy(subexpressions = identity +: subexpressions)

  /**
   * Inserts the given identity at the position specified by index, causing the elements after it to move by 1
   *
   * @param identity : the identity to add
   * @param index    : the index in the sequence of children where to add the identity
   * @return a copy of the SchemeExpression with the newly added child
   */
  def addChild(identity: Identity, index: Int): SchemeExpression[Identity] =
    copy(subexpressions =
      if (index < subexpressions.length) {
        subexpressions.take(index) ++ Seq(identity) ++ subexpressions.drop(index)
      } else
        subexpressions :+ identity
    )

  def removeChild(identity: Identity): SchemeExpression[Identity] =
    copy(subexpressions = subexpressions.filterNot(_ == identity))
}