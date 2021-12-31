package AST.CRDT

import AST.Edit.AstEdit
import AST.Node._
import boopickle.Default._

import java.nio.ByteBuffer

case class ReplicatedIntOp(editIdentity: (Int, Int), edit: AstEdit[(Int, Int)])
  extends ReplicatedOperation[(Int, Int), (Int, Int)]

object ReplicatedIntOp {
  def from(replicatedOperation: ReplicatedOperation[(Int, Int), (Int, Int)]): ReplicatedIntOp =
    ReplicatedIntOp(replicatedOperation.editIdentity, replicatedOperation.edit)

  def deserialize(buffer: ByteBuffer): ReplicatedIntOp = {
    val (editIdentity: (Int, Int), edit: AstEdit[(Int, Int)]) = Unpickle[((Int, Int), AstEdit[(Int, Int)])].fromBytes(buffer)
    ReplicatedIntOp(editIdentity, edit)
  }

  def serialize(replicatedIntOp: ReplicatedIntOp): ByteBuffer = replicatedIntOp match {
    case ReplicatedIntOp(editIdentity, edit) => Pickle.intoBytes((editIdentity, edit))
  }
}
