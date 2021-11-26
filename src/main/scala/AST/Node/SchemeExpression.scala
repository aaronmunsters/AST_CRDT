package AST.Node

case class SchemeExpression[Identity](id: Identity,
                                      parent: Option[Identity],
                                      subexpressions: Seq[Identity]) extends SchemeNode[Identity]