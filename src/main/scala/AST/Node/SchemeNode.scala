package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.TraverseOrder

sealed trait SchemeNode[Identity] {
  val start: Int
  val end: Int
  val id: Identity
  val parent: Option[Identity]

  def toAstString(implicit headedAST: HeadedAST[Identity]): String

  def height(implicit headedAST: HeadedAST[Identity]): Int

  def descendants(order: TraverseOrder)(implicit headedAST: HeadedAST[Identity]): Seq[Identity]

  def isomorphic(myHeader: HeadedAST[Identity], n: SchemeNode[Identity], otherHeader: HeadedAST[Identity]): Boolean

  def sameLabel(n: SchemeNode[Identity]): Boolean

  def sameValue(n: SchemeNode[Identity]): Boolean

  def withParent(identity: Identity): SchemeNode[Identity] // TODO: add support for F-bounded polymorphism

  def withoutChildren: SchemeNode[Identity]

  def toIdentifiedString(implicit headedAST: HeadedAST[Identity]): String
}

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

  trait LeafNode[Identity, Content] extends SchemeNode[Identity] {
    val value: Content

    def height(implicit headedAST: HeadedAST[Identity]): Int = 1

    def descendants(order: TraverseOrder)(implicit headedAST: HeadedAST[Identity]): Seq[Identity] = Seq()

    def toAstString(implicit headedAST: HeadedAST[Identity]): String = value.toString

    def isomorphic(myHeader: HeadedAST[Identity],
                   other: SchemeNode[Identity],
                   otherHeader: HeadedAST[Identity]): Boolean = sameLabel(other) && sameValue(other)

    def toIdentifiedString(implicit headedAST: HeadedAST[Identity]): String = s"<$id>$value"

    def withoutChildren: SchemeNode[Identity] = this

    def withValue(newValue: Content): LeafNode[Identity, Content]
  }

  trait RecursiveNode[Identity] extends SchemeNode[Identity] {
    def contains(identity: Identity): Boolean = children.contains(identity)

    def children: Seq[Identity]

    def height(implicit headedAST: HeadedAST[Identity]): Int =
      children.map(headedAST(_).height).foldLeft(0)(Math.max)

    def descendants(order: TraverseOrder)(implicit headedAST: HeadedAST[Identity]): Seq[Identity] = order match {
      case PreOrder => children.flatMap(child => child +: headedAST(child).descendants(order))
      case PostOrder => children.flatMap(child => headedAST(child).descendants(order) :+ child)
      case BreadthFirst => children ++ children.flatMap(headedAST(_).descendants(order))
    }

    def toAstString(implicit headedAST: HeadedAST[Identity]): String = s"(${children.map(headedAST(_).toAstString).mkString(" ")})"

    def isomorphic(myHeader: HeadedAST[Identity],
                   other: SchemeNode[Identity],
                   otherHeader: HeadedAST[Identity]): Boolean = sameLabel(other) && sameValue(other) && {
      val otherChildren = other.asInstanceOf[RecursiveNode[Identity]].children.map(otherHeader(_))
      children.size == otherChildren.size && children.map(myHeader(_)).zip(otherChildren).forall {
        case (myChildId, otherChildId) => myChildId.isomorphic(myHeader, otherChildId, otherHeader)
      }
    }
  }
}
