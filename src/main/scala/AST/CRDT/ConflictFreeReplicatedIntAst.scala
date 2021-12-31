package AST.CRDT

import AST.CRDT.ConflictFreeReplicatedIntAst.{getNodeIdGenerator, replicatedOperationOrdering}
import AST.HeadedAST.computeChanges
import AST.Node.SchemeNode.SchemeExpression
import AST.Parse.Parser
import AST.{HeadedAST, TX}

case class ConflictFreeReplicatedIntAst(final val replicaIdentity: Int, private val transmitter: TX[ReplicatedOperation[(Int, Int), (Int, Int)]])
  extends ConflictFreeReplicatedAst(
    HeadedAST.withRoot(SchemeExpression.empty(0, 0, (0, 0))),
    replicatedOperationOrdering,
    transmitter
  ) {
  private var lastEditCount = 0

  private def newEditIdentity: (Int, Int) = {
    lastEditCount += 1
    (lastEditCount, replicaIdentity)
  }

  transmitter.subscribe((changes: Seq[ReplicatedOperation[(Int, Int), (Int, Int)]]) => {
    merge(changes)
    changes
      .map(_.editIdentity)
      .map { case (newLastEditCount, _) => newLastEditCount }
      .maxOption.foreach(lastEditCount = _)
  })

  private val newNodeIdGenerator = getNodeIdGenerator(replicaIdentity)

  def update(position: Int, changedSource: String): Option[(Int, String)] = {
    for {
      updated <- Parser.parse(changedSource, newNodeIdGenerator)
      changes = computeChanges(query, updated).map(ReplicatedIntOp(newEditIdentity, _))
      if changes.nonEmpty
      _ = update(changes)
      pos = ConflictFreeReplicatedIntAst.newPos(query, position, changedSource).getOrElse(0)
    } yield (pos, query.toPrettyAstString())
  }
}

object ConflictFreeReplicatedIntAst {
  // TODO: transition editCount and Identity to value classes to lower runtime memory usage but provide more readable code
  //   => https://docs.scala-lang.org/overviews/core/value-classes.html
  //  class GlobalLamportClock(val count: Int) extends AnyVal
  //  class ReplicaIdentity(val identity: Int) extends AnyVal

  object replicatedOperationOrdering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = (x, y) match {
      case ((x_n, x_str), (y_n, y_str)) =>
        if (x_n != y_n) x_n compare y_n else x_str compare y_str
    }
  }

  def getNodeIdGenerator(replicaIdentity: Int): () => (Int, Int) = {
    var id = 0
    () => {
      id = id + 1
      (replicaIdentity, id)
    }
  }

  private def getMeaninglessNodeIdGenerator = getNodeIdGenerator(0)

  protected def newPos(headedAST: HeadedAST[(Int, Int)], oldPos: Int, oldSource: String): Option[Int] =
    headedAST.idAtConsidering(oldPos, oldSource, getMeaninglessNodeIdGenerator)
      .map { case (identity, offset) => headedAST.startPos(identity) + offset }

  def passiveUpdate(oldPosition: Int, oldSource: String, updatedReplica: ConflictFreeReplicatedIntAst): (Int, String) = {
    val newPosition = newPos(updatedReplica.query, oldPosition, oldSource).getOrElse(0)
    val newSource = updatedReplica.query.toPrettyAstString()
    (newPosition, newSource)
  }
}