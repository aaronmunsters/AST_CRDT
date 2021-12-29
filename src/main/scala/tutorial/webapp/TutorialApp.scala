package tutorial.webapp


import AST.GumTree.{GumTreeAlgorithm, MinimumEditScript}
import AST.Node.SchemeExpression
import AST.Parse.Parser
import AST.ReplicatedIntOp.serialize
import AST._

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

  @js.native
  @JSGlobal("updateSourceCode")
  def updateSourceCode(newSourceCode: String): Unit = js.native

  def main(args: Array[String]): Unit = {}

  @JSExportTopLevel("receiveRemoteUpdate")
  def receiveRemoteUpdate(data: ArrayBuffer): Unit = {
    val operation = ReplicatedIntOp.deserialize(TypedArrayBuffer.wrap(data))
    lastEditCount = operation.editIdentity._1 // update local lamport clock
    val before = local_replica.query
    local_replica.merge(Seq(operation))
    val after = local_replica.query
    if (!(before isomorphic after))
      updateSourceCode(after.toPrettyAstString())
  }

  object transmitter extends TX[ReplicatedOperation[Int, (Int, Int)]] {
    override def publish(value: Seq[ReplicatedOperation[Int, (Int, Int)]]): Unit =
      // src: https://stackoverflow.com/a/44587782
      value.map(ReplicatedIntOp.from).map(serialize).map(_.typedArray()).foreach(global_Publish)
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
    HeadedAST.withRoot(SchemeExpression.empty(0, 0, 0)),
    replicatedOperationOrdering,
    transmitter
  )

  private def updateLocalAst(after: HeadedAST[Int]): Unit = {
    val before = local_replica.query
    val mapping = GumTreeAlgorithm(before, after).mappings(before.root.get, after.root.get)
    val edits = new MinimumEditScript(before, after, mapping).compute()
    val replicableEdits = edits.map(ReplicatedIntOp(getEditIdentity(), _))
    if (replicableEdits.nonEmpty) local_replica.update(replicableEdits)
  }

  @JSExportTopLevel("sourceCodeChange")
  def sourceCodeChange(position: Int, changedSource: String): Int = {
    Parser.parse(changedSource, getIdentity).foreach(updated => {
      val before = local_replica.query
      updateLocalAst(updated)
      val after = local_replica.query
      if (!(before isomorphic after))
        updateSourceCode(after.toPrettyAstString())

    })
    var id = 0
    val uqId = () => {id+=1; id}
    local_replica.query.idAtConsidering(position, changedSource, uqId)
      .map { case (identity, offset) => local_replica.query.startPos(identity) + offset }
      .getOrElse(0)
  }
}
