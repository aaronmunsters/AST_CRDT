package AST.Node

case class SchemeIdentifier[Identity](id: Identity,
                                      parent: Option[Identity],
                                      identifier: String) extends SchemeNode[Identity]