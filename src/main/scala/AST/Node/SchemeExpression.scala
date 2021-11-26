package AST.Node

case class SchemeExpression[Identity](id: Identity,
                                      parent: Option[Identity],
                                      subexpressions: Seq[Identity]) extends SchemeNode[Identity] {
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