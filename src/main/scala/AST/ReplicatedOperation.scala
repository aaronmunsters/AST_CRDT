package AST

import AST.Edit._
import boopickle.Default._
import AST.Node._
import java.nio.ByteBuffer

trait ReplicatedOperation[Identity, EditIdentity] {
  val editIdentity: EditIdentity
  val edit: AstEdit[Identity]
}

case class ReplicatedIntOp(editIdentity: (Int, Int), edit: AstEdit[Int])
  extends ReplicatedOperation[Int, (Int, Int)]

object ReplicatedIntOp {
  def from(replicatedOperation: ReplicatedOperation[Int, (Int, Int)]): ReplicatedIntOp =
    ReplicatedIntOp(replicatedOperation.editIdentity, replicatedOperation.edit)

  def deserialize(buffer: ByteBuffer): ReplicatedIntOp = {
    val (editIdentity: (Int, Int), edit: AstEdit[Int]) = Unpickle[((Int, Int), AstEdit[Int])].fromBytes(buffer)
    ReplicatedIntOp(editIdentity, edit)
  }

  def serialize(replicatedIntOp: ReplicatedIntOp): ByteBuffer = replicatedIntOp match {
    case ReplicatedIntOp(editIdentity, edit) => Pickle.intoBytes((editIdentity, edit))
  }
}
