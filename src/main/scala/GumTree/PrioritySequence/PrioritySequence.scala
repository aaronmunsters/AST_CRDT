package GumTree.PrioritySequence

import GumTree.InMemoryAst.Node.{SchemeExpression, SchemeNode}

import scala.collection.mutable

class PrioritySequence[Identity] {
  implicit object NodeOrdering extends Ordering[SchemeNode[Identity]] {
    def compare(a: SchemeNode[Identity], b: SchemeNode[Identity]): Int =
      a.height compare b.height
  }

  var queue: mutable.PriorityQueue[SchemeNode[Identity]] =
    new mutable.PriorityQueue[SchemeNode[Identity]]()

  /** Add tree in the queue */
  def push(tree: SchemeNode[Identity]): Unit =
    queue.enqueue(tree)

  /** Return greatest height of the list */
  def peekMax: Option[Int] = queue.headOption.map(_.height)

  /** Return set of all nodes of a height equal to `peekMax` */
  def pop: Option[Set[SchemeNode[Identity]]] = {
    if (queue.isEmpty) return None
    val height = peekMax.get
    var result = Set(queue.dequeue())
    while (queue.nonEmpty && peekMax.get == height)
      result = Set(queue.dequeue()) union result
    Some(result)
  }

  /** Inserts all children */
  def open(tree: SchemeNode[Identity]): Unit = {
    tree match {
      case expression: SchemeExpression[Identity] =>
        expression.subexpressions.foreach(queue.enqueue(_))
      case _ =>
    }
  }
}
