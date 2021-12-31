package AST.CRDT

import AST.CRDT.ReplicatedIntOp.{LClock, NId, RId}
import AST.Edit.AstEdit
import AST.Node._
import boopickle.Default._

import java.nio.ByteBuffer

case class ReplicatedIntOp(editIdentity: (LClock, RId), edit: AstEdit[(RId, NId)])
  extends ReplicatedOperation[(RId, NId), (LClock, RId)]

object ReplicatedIntOp {
  // Here the use of value classes allows for compile-time safety while keeping a low memory footprint
  // src: https://docs.scala-lang.org/overviews/core/value-classes.html & https://stackoverflow.com/a/17658744
  case class NId(id: Int) extends AnyVal

  case class LClock(count: Int) extends AnyVal

  case class RId(identity: Int) extends AnyVal

  def from(replicatedOperation: ReplicatedOperation[(RId, NId), (LClock, RId)]): ReplicatedIntOp =
    ReplicatedIntOp(replicatedOperation.editIdentity, replicatedOperation.edit)

  def deserialize(buffer: ByteBuffer): ReplicatedIntOp = {
    val (editIdentity: (LClock, RId), edit: AstEdit[(RId, NId)]) =
      Unpickle[((LClock, RId), AstEdit[(RId, NId)])].fromBytes(buffer)
    ReplicatedIntOp(editIdentity, edit)
  }

  def serialize(replicatedIntOp: ReplicatedIntOp): ByteBuffer = replicatedIntOp match {
    case ReplicatedIntOp(editIdentity, edit) => Pickle.intoBytes((editIdentity, edit))
  }
}
