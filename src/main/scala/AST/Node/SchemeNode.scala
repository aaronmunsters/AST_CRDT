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

  sealed trait LeafNode[Identity, Content] extends SchemeNode[Identity] {
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

  sealed trait RecursiveNode[Identity] extends SchemeNode[Identity] {
    def contains(identity: Identity): Boolean = children.contains(identity)

    def children: Seq[Identity]

    def height(implicit headedAST: HeadedAST[Identity]): Int =
      children.map(headedAST(_).height).foldLeft(0)(Math.max) + 1

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

    def withChildren(children: Seq[Identity]): RecursiveNode[Identity]

    def removeChild(child: Identity): RecursiveNode[Identity]
  }

  // Start of instantiations
  case class SchemeExpression[Identity](start: Int,
                                        end: Int,
                                        id: Identity,
                                        parent: Option[Identity],
                                        children: Seq[Identity]) extends RecursiveNode[Identity] {
    def prependChild(identity: Identity): SchemeExpression[Identity] =
      copy(children = identity +: children)

    /**
     * Inserts the given identity at the position specified by index, causing the elements after it to move by 1
     *
     * @param identity : the identity to add
     * @param index    : the index in the sequence of children where to add the identity
     * @return a copy of the SchemeExpression with the newly added child
     */
    def addChild(identity: Identity, index: Int): SchemeExpression[Identity] =
      copy(children =
        if (index < children.length) {
          children.take(index).filterNot(_ == identity) ++ Seq(identity) ++ children.drop(index).filterNot(_ == identity)
        } else
          children.filterNot(_ == identity) :+ identity
      )

    def removeChild(identity: Identity): SchemeExpression[Identity] =
      copy(children = children.filterNot(_ == identity))

    override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
      case _: SchemeExpression[_] => true
      case _ => false
    }

    override def sameValue(n: SchemeNode[Identity]): Boolean = sameLabel(n)

    override def withParent(identity: Identity): SchemeNode[Identity] =
      copy(parent = Some(identity))

    def toIdentifiedString(implicit headedAST: HeadedAST[Identity]): String =
      s"(<$id>-${children.map(headedAST(_).toIdentifiedString).mkString(" ")}-<$id>)"

    def withoutChildren: SchemeNode[Identity] = copy(children = Seq())

    override def withChildren(children: Seq[Identity]): RecursiveNode[Identity] =
      copy(children = children)
  }

  object SchemeExpression {
    def empty[Identity](start: Int, end: Int, identity: Identity): SchemeExpression[Identity] =
      AST.Node.SchemeNode.SchemeExpression(start, end, identity, None, Seq())
  }

  case class SchemeIdentifier[Identity](start: Int,
                                        end: Int,
                                        id: Identity,
                                        parent: Option[Identity],
                                        value: Seq[Char]) extends LeafNode[Identity, Seq[Char]] {
    override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
      case _: SchemeIdentifier[_] => true
      case _ => false
    }

    override def sameValue(n: SchemeNode[Identity]): Boolean =
      sameLabel(n) && n.asInstanceOf[SchemeIdentifier[Identity]].value == value

    override def withParent(identity: Identity): SchemeNode[Identity] =
      copy(parent = Some(identity))

    override def withValue(newValue: Seq[Char]): SchemeIdentifier[Identity] = this.copy(value = newValue)

    override def toAstString(implicit headedAST: HeadedAST[Identity]): String = value.mkString
  }

  case class SchemeNumber[Identity](start: Int,
                                    end: Int,
                                    id: Identity,
                                    parent: Option[Identity],
                                    value: Long) extends LeafNode[Identity, Long] {
    override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
      case _: SchemeNumber[_] => true
      case _ => false
    }

    override def sameValue(n: SchemeNode[Identity]): Boolean =
      sameLabel(n) && n.asInstanceOf[SchemeNumber[Identity]].value == value

    override def withParent(identity: Identity): SchemeNode[Identity] =
      copy(parent = Some(identity))

    override def withValue(newValue: Long): SchemeNumber[Identity] = this.copy(value = newValue)
  }

  case class SchemeString[Identity](start: Int,
                                    end: Int,
                                    id: Identity,
                                    parent: Option[Identity],
                                    value: Seq[Char]) extends LeafNode[Identity, Seq[Char]] {
    override def toAstString(implicit headedAST: HeadedAST[Identity]): String = '"' + value.mkString + '"'

    override def sameLabel(n: SchemeNode[Identity]): Boolean = n match {
      case _: SchemeString[_] => true
      case _ => false
    }

    override def sameValue(n: SchemeNode[Identity]): Boolean =
      sameLabel(n) && n.asInstanceOf[SchemeString[Identity]].value == value

    override def withParent(identity: Identity): SchemeNode[Identity] =
      copy(parent = Some(identity))

    override def withValue(newValue: Seq[Char]): SchemeString[Identity] = this.copy(value = newValue)
  }

}
