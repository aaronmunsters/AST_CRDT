package AST.CRDT

import AST.CRDT.ConflictFreeReplicatedIntAst._
import AST.CRDT.ReplicatedIntOp.{LClock, NId, RId}
import AST.HeadedAST.computeChanges
import AST.Node.SchemeNode.SchemeExpression
import AST.Parse.Parser
import AST.{HeadedAST, TX}

case class ConflictFreeReplicatedIntAst(final val replicaIdentity: RId,
                                        private val transmitter: TX[ReplicatedIntOp])
  extends ConflictFreeReplicatedAst[(RId, NId), (LClock, RId)](
    HeadedAST.withRoot(SchemeExpression.empty(0, 0, (RId(0), NId(0)))),
    replicatedOperationOrdering,
    transmitter.asInstanceOf[TX[ReplicatedOperation[(RId, NId), (LClock, RId)]]]
  ) {
  private var lastEditCount = 0

  private def newEditIdentity: (LClock, RId) = {
    lastEditCount += 1
    (LClock(lastEditCount), replicaIdentity)
  }

  transmitter.subscribe((changes: Seq[ReplicatedOperation[(RId, NId), (LClock, RId)]]) => {
    merge(changes)
    changes
      .map(_.editIdentity)
      .map { case (LClock(count), _) => count }
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
  object replicatedOperationOrdering extends Ordering[(LClock, RId)] {
    override def compare(x: (LClock, RId), y: (LClock, RId)): Int = (x, y) match {
      case ((x_n, x_str), (y_n, y_str)) =>
        if (x_n.count != y_n.count) x_n.count compare y_n.count else x_str.identity compare y_str.identity
    }
  }

  def getNodeIdGenerator(replicaIdentity: RId): () => (RId, NId) = {
    var id = 0
    () => {
      id = id + 1
      (replicaIdentity, NId(id))
    }
  }

  private def getMeaninglessNodeIdGenerator =
    getNodeIdGenerator(RId(0))

  protected def newPos(headedAST: HeadedAST[(RId, NId)], oldPos: Int, oldSource: String): Option[Int] =
    headedAST.idAtConsidering(oldPos, oldSource, getMeaninglessNodeIdGenerator)
      .map { case (identity, offset) => headedAST.startPos(identity) + offset }

  def passiveUpdate(oldPosition: Int, oldSource: String, updatedReplica: ConflictFreeReplicatedIntAst): (Int, String) = {
    val newPosition = newPos(updatedReplica.query, oldPosition, oldSource).getOrElse(0)
    val newSource = updatedReplica.query.toPrettyAstString()
    (newPosition, newSource)
  }
}