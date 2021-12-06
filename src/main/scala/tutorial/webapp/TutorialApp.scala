package tutorial.webapp


import AST.GumTree.{GumTreeAlgorithm, MinimumEditScript}
import AST.Node.SchemeExpression
import AST.Parse.Parser
import AST._
import org.scalajs.dom.document
import org.scalajs.dom.html.TextArea

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobal}
import scala.scalajs.js.typedarray.TypedArrayBufferOps.bufferOps
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.util.Random

case class Operation(name: String, age: Int) extends Serializable

object TutorialApp {
  @js.native
  @JSGlobal("publish")
  def global_Publish(v: Any): Unit = js.native

  def main(args: Array[String]): Unit = {}

  @JSExportTopLevel("receiveRemoteUpdate")
  def receiveRemoteUpdate(data: ArrayBuffer): Unit = {
    val operation = ReplicatedStringOp.deserialize(TypedArrayBuffer.wrap(data))
    lastEditCount = operation.editIdentity._1 // update local lamport clock
    val before = local_replica.query
    local_replica.merge(Seq(operation))
    val after = local_replica.query
    if (!(before isomorphic after))
      sourceCodeContainer.value = after.toPrettyAstString()
  }

  object transmitter extends TX[ReplicatedOperation[Int, (Int, Int)]] {
    override def publish(value: Seq[ReplicatedOperation[Int, (Int, Int)]]): Unit = {
      // src: https://stackoverflow.com/a/44587782
      value.map(ReplicatedStringOp.from).map(_.serialize().typedArray()).foreach(global_Publish)
    }
  }

  object replicatedOperationOrdering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = (x, y) match {
      case ((x_n, x_str), (y_n, y_str)) =>
        if (x_n != y_n) x_n compare y_n else x_str compare y_str
    }
  }

  private var identityCount: Int = 0
  private val deviceIdentity: Int = new Random().nextInt()
  private val getIdentity = () => {
    identityCount += 1
    identityCount // TODO: incorporate the deviceIdentity
  }
  private var lastEditCount: Int = 0
  private val getEditIdentity = () => {
    lastEditCount += 1
    (lastEditCount, deviceIdentity)
  }
  private val local_replica = new ConflictFreeReplicatedAst[Int, (Int, Int)](
    HeadedAST.withRoot(SchemeExpression(0, None, Seq())),
    replicatedOperationOrdering,
    transmitter
  )

  private def updateLocalAst(after: HeadedAST[Int]): Unit = {
    val before = local_replica.query
    val mapping = GumTreeAlgorithm(before, after).mappings(before.root.get, after.root.get)
    val edits = new MinimumEditScript(before, after, mapping).compute()
    val replicableEdits = edits.map(ReplicatedStringOp(getEditIdentity(), _))
    if (replicableEdits.nonEmpty) local_replica.update(replicableEdits)
  }

  private val sourceCodeContainer: TextArea = document.getElementById("source-code-area").asInstanceOf[TextArea]

  @JSExportTopLevel("sourceCodeChange")
  def sourceCodeChange(): Unit = {

    val sourceCode = sourceCodeContainer.value
    Parser.parseSchemeSmall(sourceCode, getIdentity).foreach(updated => {
      val before = local_replica.query
      updateLocalAst(updated)
      val after = local_replica.query
      if (!(before isomorphic after))
        sourceCodeContainer.value = after.toPrettyAstString()
    })
  }
}
