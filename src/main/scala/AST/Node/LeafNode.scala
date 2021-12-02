package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.TraverseOrder

trait LeafNode[Identity, Content] extends SchemeNode[Identity] {
  val value: Content

  def height(implicit headedAST: HeadedAST[Identity]): Int = 1

  def descendants(order: TraverseOrder)(implicit headedAST: HeadedAST[Identity]): Seq[Identity] = Seq()

  def toAstString(implicit headedAST: HeadedAST[Identity]): String = value.toString

  def isomorphic(myHeader: HeadedAST[Identity],
                 other: SchemeNode[Identity],
                 otherHeader: HeadedAST[Identity]): Boolean = sameLabel(other) && sameValue(other)

  def toIdentifiedString(implicit headedAST: HeadedAST[Identity]): String = s"<$id>$value"
}
