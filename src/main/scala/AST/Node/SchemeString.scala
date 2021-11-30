package AST.Node

import AST.HeadedAST

case class SchemeString[Identity](id: Identity,
                                  parent: Option[Identity],
                                  value: String) extends LeafNode[Identity, String] {
  override def toAstString(implicit headedAST: HeadedAST[Identity]): String = '"' + value + '"'

  override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
    case _: SchemeString[_] => true
    case _ => false
  }

  override def sameValue(n: SchemeNode[Identity]): Boolean =
    sameLabel(n) && n.asInstanceOf[SchemeString[Identity]].value == value
}
