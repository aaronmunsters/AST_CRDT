package AST.GumTree

import AST.HeadedAST
import AST.Node.{RecursiveNode, SchemeNode}

import scala.collection.mutable

class PrioritySequence[Identity](headedAST: HeadedAST[Identity]) {
  implicit val ast: HeadedAST[Identity] = headedAST

  implicit object NodeOrdering extends Ordering[Identity] {
    def compare(a: Identity, b: Identity): Int =
      ast(a).height compare ast(b).height
  }

  var queue: mutable.PriorityQueue[Identity] =
    new mutable.PriorityQueue[Identity]()

  /** Add tree in the queue */
  def push(tree: Identity): Unit =
    queue.enqueue(tree)

  /** Return greatest height of the list */
  def peekMax: Int =
    queue.headOption.map(ast(_).height).getOrElse(Int.MinValue)

  /** Return set of all nodes of a height equal to `peekMax` */
  def pop: Set[Identity] = {
    if (queue.isEmpty) return Set.empty
    val height = peekMax
    var result = Set(queue.dequeue())
    while (queue.nonEmpty && peekMax == height)
      result = Set(queue.dequeue()) union result
    result
  }

  /** Inserts all children */
  def open(tree: SchemeNode[Identity]): Unit =
    tree match {
      case node: RecursiveNode[Identity] => node.children.foreach(queue.enqueue(_))
      case _ =>
    }

  override def toString: String =
    s"PrioritySequence[${queue.toSeq.map(headedAST(_)).map(n => s"${n.id}-> ${n.toAstString.take(10)}...").mkString(", ")}]"
}
