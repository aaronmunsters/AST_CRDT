package tutorial.webapp

import AST.CRDT.ReplicatedIntOp.serialize
import AST.CRDT.{ConflictFreeReplicatedIntAst, ReplicatedIntOp, ReplicatedOperation}
import AST._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import scala.scalajs.js.typedarray.TypedArrayBufferOps.bufferOps
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.util.Random

object TutorialApp {
  def main(args: Array[String]): Unit = {}

  @js.native
  @JSGlobal("publish")
  def global_publish(v: Any): Unit = js.native

  @js.native
  @JSGlobal("updateSourceCode")
  def updateSourceCode(position: Int, newSourceCode: String): Unit = js.native

  object PeerJS_Transmitter extends TX[ReplicatedOperation[(Int, Int), (Int, Int)]] {
    var local_callback: Option[Seq[ReplicatedOperation[(Int, Int), (Int, Int)]] => Unit] = None

    override def publish(value: Seq[ReplicatedOperation[(Int, Int), (Int, Int)]]): Unit =
      value.asInstanceOf[Seq[ReplicatedIntOp]].map(serialize).map(_.typedArray()).foreach(global_publish)

    override def subscribe(callback: Seq[ReplicatedOperation[(Int, Int), (Int, Int)]] => Unit): Unit =
      local_callback = Some(callback)
  }

  private val localReplica = ConflictFreeReplicatedIntAst(new Random().nextInt(), PeerJS_Transmitter)

  @JSExportTopLevel("receiveRemoteUpdate")
  def receiveRemoteUpdate(data: ArrayBuffer): Unit = {
    val operation = ReplicatedIntOp.deserialize(TypedArrayBuffer.wrap(data))
    PeerJS_Transmitter.local_callback.get(Seq(operation))
    // TODO: provide old position before update takes place
    updateSourceCode(0, localReplica.query.toPrettyAstString())
  }

  @JSExportTopLevel("sourceCodeChange")
  def sourceCodeChange(position: Int, changedSource: String): Unit =
    localReplica.update(position, changedSource).foreach {
      case (newPos, newSource) => updateSourceCode(newPos, newSource)
    }
}
