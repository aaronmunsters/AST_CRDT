package AST.Node

case class SchemeNumber[Identity](id: Identity,
                                  parent: Option[Identity],
                                  number: Long) extends SchemeNode[Identity]
