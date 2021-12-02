package AST.Node

case class SchemeIdentifier[Identity](id: Identity,
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
}