package webapp

import AST.CRDT.ReplicatedIntOp.{RId, serialize}
import AST.CRDT.{ConflictFreeReplicatedIntAst, ReplicatedIntOp}
import AST.TX

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import scala.scalajs.js.typedarray.TypedArrayBufferOps.bufferOps
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.util.Random

object CRDT_AST_ScalaJS {
  def main(args: Array[String]): Unit = {}

  @js.native
  @JSGlobal("publish")
  def global_publish(v: Any): Unit = js.native

  @js.native
  @JSGlobal("updateSourceCode")
  def updateSourceCode(position: Int, newSourceCode: String): Unit = js.native

  object PeerJS_Transmitter extends TX[ReplicatedIntOp] {
    var local_callback: Option[Seq[ReplicatedIntOp] => Unit] = None

    override def publish(value: Seq[ReplicatedIntOp]): Unit =
      value.map(serialize).map(_.typedArray()).foreach(global_publish)

    override def subscribe(callback: Seq[ReplicatedIntOp] => Unit): Unit =
      local_callback = Some(callback)
  }

  private val localReplica = ConflictFreeReplicatedIntAst(RId(new Random().nextInt()), PeerJS_Transmitter)

  @JSExportTopLevel("receiveRemoteUpdate")
  def receiveRemoteUpdate(oldPosition: Int, data: ArrayBuffer): Unit = {
    val oldSource = localReplica.query.toPrettyAstString()
    val operation = ReplicatedIntOp.deserialize(TypedArrayBuffer.wrap(data))
    PeerJS_Transmitter.local_callback.get(Seq(operation))
    val (newPosition, newSource) = ConflictFreeReplicatedIntAst.passiveUpdate(oldPosition, oldSource, localReplica)
    updateSourceCode(newPosition, newSource)
  }

  @JSExportTopLevel("sourceCodeChange")
  def sourceCodeChange(position: Int, changedSource: String): Unit =
    localReplica.update(position, changedSource).foreach {
      case (newPos, newSource) => updateSourceCode(newPos, newSource)
    }
}
