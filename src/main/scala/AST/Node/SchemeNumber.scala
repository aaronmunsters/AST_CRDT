package AST.Node

import AST.Node.SchemeNode.LeafNode

case class SchemeNumber[Identity](id: Identity,
                                  parent: Option[Identity],
                                  value: Long) extends LeafNode[Identity, Long] {
  override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
    case _: SchemeNumber[_] => true
    case _ => false
  }

  override def sameValue(n: SchemeNode[Identity]): Boolean =
    sameLabel(n) && n.asInstanceOf[SchemeNumber[Identity]].value == value

  override def withParent(identity: Identity): SchemeNode[Identity] =
    copy(parent = Some(identity))

  override def withValue(newValue: Long): SchemeNumber[Identity] = this.copy(value = newValue)
}
