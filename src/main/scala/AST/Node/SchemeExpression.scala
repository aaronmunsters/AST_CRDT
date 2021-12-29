package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.RecursiveNode

object SchemeExpression {
  def empty[Identity](start: Int, end: Int, identity: Identity): SchemeExpression[Identity] =
    SchemeExpression(start, end, identity, None, Seq())
}

case class SchemeExpression[Identity](start: Int,
                                      end: Int,
                                      id: Identity,
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
        children.take(index).filterNot(_ == identity) ++ Seq(identity) ++ children.drop(index).filterNot(_ == identity)
      } else
        children.filterNot(_ == identity) :+ identity
    )

  def removeChild(identity: Identity): SchemeExpression[Identity] =
    copy(children = children.filterNot(_ == identity))

  override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
    case _: SchemeExpression[_] => true
    case _ => false
  }

  override def sameValue(n: SchemeNode[Identity]): Boolean = sameLabel(n)

  override def withParent(identity: Identity): SchemeNode[Identity] =
    copy(parent = Some(identity))

  def toIdentifiedString(implicit headedAST: HeadedAST[Identity]): String =
    s"(<$id>-${children.map(headedAST(_).toIdentifiedString).mkString(" ")}-<$id>)"

  def withoutChildren: SchemeNode[Identity] = copy(children = Seq())

  override def withChildren(children: Seq[Identity]): RecursiveNode[Identity] =
    copy(children = children)
}