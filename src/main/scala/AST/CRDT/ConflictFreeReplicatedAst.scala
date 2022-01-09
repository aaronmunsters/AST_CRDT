package AST.CRDT

import AST.{HeadedAST, TX}

class ConflictFreeReplicatedAst[Identity, EditIdentity](var start: HeadedAST[Identity],
                                                        implicit val ordering: Ordering[EditIdentity],
                                                        transmitter: TX[ReplicatedOperation[Identity, EditIdentity]]) {

  // replicatedOperationOrdering --> means to order the operations
  implicit val replicatedOperationOrdering: Ordering[ReplicatedOperation[Identity, EditIdentity]] = Ordering.by(_.editIdentity)
  // the set of operations that, together with the `start`, allow us to construct the replicated AST
  var operations: Seq[ReplicatedOperation[Identity, EditIdentity]] = Seq()

  // Gets called whenever a local change takes place
  def update(newOperations: Seq[ReplicatedOperation[Identity, EditIdentity]]): Unit = {
    operations = (operations ++ newOperations).sorted
    transmitter.publish(newOperations)
  }

  // Gets called whenever the data structure needs to be viewed
  def query: HeadedAST[Identity] =
    operations.map(_.edit).foldLeft(start)((ast, op) => ast perform op)

  // Gets called whenever new operations come in over the wire
  def merge(newOperations: Seq[ReplicatedOperation[Identity, EditIdentity]]): Unit =
    operations = (operations ++ newOperations).sorted
}