package AST

import AST.Edit._
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNode, SchemeNumber}
import AST.ReplicatedStringOp.encodeTree
import boopickle.Default._

import java.nio.ByteBuffer

trait ReplicatedOperation[Identity, EditIdentity] {
  val editIdentity: EditIdentity
  val edit: AstEdit[Identity]
}

protected case class RepOp(ei: (Int, Int), e: Array[Int], tree: Option[Array[Int]])

case class ReplicatedStringOp(editIdentity: (Int, Int), edit: AstEdit[Int])
  extends ReplicatedOperation[Int, (Int, Int)] {

  def serialize(): ByteBuffer = {
    val repOp: RepOp = edit match {
      case add: AstEdit.Add[Int] if add.parent.isEmpty =>
        val e = Array(1, add.index, 0, 0)
        RepOp(this.editIdentity, e, Some(encodeTree(add.tree)))
      case add: AstEdit.Add[Int] if add.parent.nonEmpty =>
        val e = Array(1, add.index, add.parent.get, 1)
        RepOp(this.editIdentity, e, Some(encodeTree(add.tree)))
      case delete: AstEdit.Delete[Int] =>
        val e = Array(2, delete.target)
        RepOp(this.editIdentity, e, None)
      case move: AstEdit.Move[Int] =>
        val e = Array(3, move.child, move.newParent, move.index)
        RepOp(this.editIdentity, e, None)
      case value: AstEdit.UpdateValue[Int] =>
        val e = Array(4, value.target, value.value.asInstanceOf[Long].toInt)
        RepOp(this.editIdentity, e, None)
    }

    Pickle.intoBytes(repOp)
  }

}

object ReplicatedStringOp {
  def from(v: ReplicatedOperation[Int, (Int, Int)]): ReplicatedStringOp =
    ReplicatedStringOp(v.editIdentity, v.edit)

  private def encodeTree(node: SchemeNode[Int]): Array[Int] = node match {
    case node: SchemeNumber[Int] if node.parent.nonEmpty => Array(1, node.id, node.value.toInt, 1, node.parent.get)
    case node: SchemeNumber[Int] if node.parent.isEmpty => Array(1, node.id, node.value.toInt, 0, 0)
    case node: SchemeExpression[Int] if node.parent.nonEmpty => Array(2, node.id, 1, node.parent.get) ++ node.children
    case node: SchemeExpression[Int] if node.parent.nonEmpty => Array(2, node.id, 0, 0) ++ node.children
  }

  protected def decodeTree(node: Array[Int]): SchemeNode[Int] = node match {
    case Array(1, id, value, 1, parent) => SchemeNumber(id, Some(parent), value)
    case Array(1, id, value, 0, 0) => SchemeNumber(id, None, value)
    case Array(2, id, 1, parent, children@_*) => SchemeExpression(id, Some(parent), children)
    case Array(2, id, 0, 0, children@_*) => SchemeExpression(id, None, children)
  }


  def deserialize(buffer: ByteBuffer): ReplicatedStringOp = {
    val RepOp(ei, e, target) = Unpickle[RepOp].fromBytes(buffer)
    val edit = e match {
      case Array(1, index, 0, 0) => Add(decodeTree(target.get), None, index)
      case Array(1, index, parent, 1) => Add(decodeTree(target.get), Some(parent), index)
      case Array(2, target) => Delete(target)
      case Array(3, child, newParent, index) => Move(child, newParent, index)
      case Array(4, target, value) => UpdateValue(target, value.toLong)
    }
    ReplicatedStringOp(ei, edit)
  }

}