package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.TraverseOrder

object SchemeNode {
  /*         A
           / | \
          /  |  \
         /   |   \
        /    |    \
       /     |     \
      B      C      D
     / \    / \    / \
    E   F  G   H  I   J         */
  sealed trait TraverseOrder

  // A B E F C G H D I J
  final object PreOrder extends TraverseOrder

  // E F B G H C I J D A
  final object PostOrder extends TraverseOrder

  // A B C D E F G H I J
  final object BreadthFirst extends TraverseOrder
}

trait SchemeNode[Identity] {
  val id: Identity
  val parent: Option[Identity]

  def toAstString(implicit headedAST: HeadedAST[Identity]): String

  def height(implicit headedAST: HeadedAST[Identity]): Int

  def descendants(order: TraverseOrder)(implicit headedAST: HeadedAST[Identity]): Seq[Identity]

  def isomorphic(myHeader: HeadedAST[Identity], n: SchemeNode[Identity], otherHeader: HeadedAST[Identity]): Boolean

  def sameLabel(n: SchemeNode[Identity]): Boolean

  def sameValue(n: SchemeNode[Identity]): Boolean

  // And maybe for debugging:
  //  def toIdentifiedString: String
}
