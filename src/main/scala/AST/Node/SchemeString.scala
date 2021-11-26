package AST.Node

case class SchemeString[Identity](id: Identity,
                                  parent: Option[Identity],
                                  content: String) extends SchemeNode[Identity]
