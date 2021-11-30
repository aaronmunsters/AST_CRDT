package AST.Node

import AST.HeadedAST
import AST.Node.SchemeNode.{BreadthFirst, PostOrder, PreOrder, TraverseOrder}

trait RecursiveNode[Identity] extends SchemeNode[Identity] {
  def contains(identity: Identity): Boolean = children.contains(identity)

  def children: Seq[Identity]

  def height(implicit headedAST: HeadedAST[Identity]): Int = children.map(headedAST(_)).map[Int](_.height).max

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
