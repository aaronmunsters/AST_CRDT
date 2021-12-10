package AST.Node

import AST.Node.SchemeNode.LeafNode

case class SchemeIdentifier[Identity](start: Int,
                                      end: Int,
                                      id: Identity,
                                      parent: Option[Identity],
                                      value: String) extends LeafNode[Identity, String] {
  override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
    case _: SchemeIdentifier[_] => true
    case _ => false
  }

  override def sameValue(n: SchemeNode[Identity]): Boolean =
    sameLabel(n) && n.asInstanceOf[SchemeIdentifier[Identity]].value == value

  override def withParent(identity: Identity): SchemeNode[Identity] =
    copy(parent = Some(identity))

  override def withValue(newValue: String): SchemeIdentifier[Identity] = this.copy(value = newValue)
}