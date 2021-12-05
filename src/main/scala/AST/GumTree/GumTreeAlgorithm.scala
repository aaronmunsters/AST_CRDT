package AST.GumTree

import AST.HeadedAST
import AST.Node.SchemeNode.PostOrder

import scala.collection.mutable

case class GumTreeAlgorithm[Identity](T1_Header: HeadedAST[Identity], T2_Header: HeadedAST[Identity]) {
  private val T1_nodes = descendants(T1_Header.root.get) :+ T1_Header.root.get
  private val T2_nodes = descendants(T2_Header.root.get) :+ T2_Header.root.get

  private def descendants(identity: Identity): Seq[Identity] =
    if (T1_Header.contains(identity))
      T1_Header(identity).descendants(PostOrder)(T1_Header)
    else
      T2_Header(identity).descendants(PostOrder)(T2_Header)

  private def isomorphic(t1_id: Identity, t2_id: Identity): Boolean =
    T1_Header(t1_id).isomorphic(T1_Header, T2_Header(t2_id), T2_Header)

  def dice(t1_id: Identity, t2_id: Identity, mapping: mutable.Map[Identity, Identity]): Float = {
    val t1_descendants = descendants(t1_id)
    val t2_descendants = descendants(t2_id)
    if (t1_descendants.isEmpty && t2_descendants.isEmpty)
      return if (isomorphic(t1_id, t2_id)) 1 else 0
    val common_descendants =
      for {t1_desc <- t1_descendants
           t2_desc <- t2_descendants
           if mapping.exists { case (`t1_desc`, `t2_desc`) => true; case _ => false }} yield (t1_desc, t2_desc)
    (2 * common_descendants.size).toFloat / (t1_descendants.size + t2_descendants.size).toFloat
  }

  private def open(identity: Identity, sequence: PrioritySequence[Identity]): Unit =
    sequence.open(if (T1_Header.contains(identity)) T1_Header(identity) else T2_Header(identity))

  private def pop(sequence: PrioritySequence[Identity]): Set[Identity] = sequence.pop

  private def add(map: mutable.Map[Identity, Identity], isomorphic: (Identity, Identity)): Unit = isomorphic match {
    case (t1, t2) => map.update(t1, t2)
  }

  private def sameLabel(t1: Identity, t2: Identity): Boolean =
    T1_Header(t1) sameLabel T2_Header(t2)

  private def parent(identity: Identity): Identity =
    if (T1_Header.contains(identity))
      T1_Header(identity).parent.get
    else
      T2_Header(identity).parent.get

  private def postOrder(identity: Identity): Seq[Identity] = descendants(identity) :+ identity

  def isomorphicDescendants(t1_id: Identity, t2_id: Identity): Seq[(Identity, Identity)] =
    descendants(t1_id) zip descendants(t2_id)

  // Alternations to the paper:
  // - Checking if L1/L2 queue are non-empty
  //    => the authors do this too in their implementation:
  //       https://github.com/GumTreeDiff/gumtree/blob/553ba7f7a6c8516cd8eea41e5536a6609baf900b/core/src/main/java/com/github/gumtreediff/matchers/heuristic/gt/AbstractSubtreeMatcher.java#L68
  // - L1/L2 their minimal maxHeight must be greater OR EQUAL to minHeight
  // - For candidate t1 and t2 that are isomorphic with no alternate candidates, they too are added to M
  //    => the authors do this too in their implementation:
  //       https://github.com/GumTreeDiff/gumtree/blob/553ba7f7a6c8516cd8eea41e5536a6609baf900b/core/src/main/java/com/github/gumtreediff/matchers/heuristic/gt/AbstractSubtreeMatcher.java#L80
  def topDown(T1: Identity,
              T2: Identity,
              minHeight: Int,
              L1: PrioritySequence[Identity],
              L2: PrioritySequence[Identity],
              A: mutable.Map[Identity, Identity],
              M: mutable.Map[Identity, Identity]):
  mutable.Map[Identity, Identity] = {
    L1.push(T1)
    L2.push(T2)
    while (Math.min(L1.peekMax, L2.peekMax) >= minHeight) {
      if (L1.peekMax != L2.peekMax)
        if (L1.peekMax > L2.peekMax) {
          pop(L1).foreach(t => open(t, L1))
        } else {
          pop(L2).foreach(t => open(t, L2))
        } else {
        val H1 = pop(L1)
        val H2 = pop(L2)
        for {t1 <- H1; t2 <- H2} {
          if (isomorphic(t1, t2)) {
            if (descendants(T2).exists(tx => tx != t2 && isomorphic(t1, tx)) ||
              descendants(T1).exists(tx => tx != t1 && isomorphic(tx, t2))) {
              add(A, (t1, t2))
            } else {
              ((t1, t2) +: isomorphicDescendants(t1, t2)) foreach { case (t1, t2) => add(M, (t1, t2)) }
            }
          }
        }
        for {t1 <- H1}
          if (!(A ++ M).keys.toSeq.contains(t1)) L1.open(T1_Header(t1))
        for {t2 <- H2}
          if (!(A ++ M).values.toSeq.contains(t2)) L2.open(T2_Header(t2))
      }
    }

    var sortedA = A.toSeq.sortBy { case (t1, t2) =>
      dice(
        parent(t1),
        parent(t2),
        M
      )
    }
    while (sortedA.nonEmpty) {
      val (t1, t2) :: tailA = sortedA
      ((t1, t2) +: isomorphicDescendants(t1, t2)) foreach { case (t1, t2) => add(M, (t1, t2)) }
      sortedA = tailA.filterNot { case (k, _) => k == t1 }
      sortedA = tailA.filterNot { case (_, v) => v == t2 }
    }
    M
  }

  def opt(T1: Identity,
          T2: Identity):
  Map[Identity, Identity] =
    Map(T1 -> T2) // TODO: replace by a serious implementation, eg. RTED

  def bottomUp(T1_id: Identity,
               T2_id: Identity,
               minDice: Float,
               maxSize: Int,
               M: mutable.Map[Identity, Identity]):
  mutable.Map[Identity, Identity] = {
    def candidate(t1: Identity, M: mutable.Map[Identity, Identity]): Option[Identity] = {
      T2_nodes
        .filter(sameLabel(t1, _))
        .filter(!M.values.toSeq.contains(_))
        // 3rd condition from the paper; "have some matching descendants" is interpreted as "highest dice correlation"
        .sortBy(-dice(t1, _, M))
        .headOption
    }

    postOrder(T1_id).foreach(t1 => {
      if (!M.contains(t1) && descendants(t1).exists(M.contains)) {
        candidate(t1, M) match {
          case Some(t2) if dice(t1, t2, M) >= minDice =>
            M.update(t1, t2)
            if (Math.max(descendants(t1).size, descendants(t2).size) < maxSize) {
              val R = opt(t1, t2)
              for ((ta, tb) <- R)
                if (M.exists((ta, tb) == _) && sameLabel(ta, tb))
                  M.update(ta, tb)
            }
          case None => // Do nothing
        }
      }
    })
    M
  }

  def mappings(T1: Identity,
               T2: Identity,
               bottomUpMinHeight: Int = 1,
               topDownMinDice: Float = 0,
               topDownMaxSize: Int = 100): mutable.Map[Identity, Identity] = {

    val topDownMappings = topDown(
      T1,
      T2,
      bottomUpMinHeight,
      new PrioritySequence[Identity](T1_Header),
      new PrioritySequence[Identity](T2_Header),
      scala.collection.mutable.Map.empty[Identity, Identity],
      scala.collection.mutable.Map.empty[Identity, Identity]
    )

    val withBottomUpMappings = bottomUp(
      T1,
      T2,
      topDownMinDice,
      topDownMaxSize,
      topDownMappings
    )

    withBottomUpMappings.update(T1, T2)
    withBottomUpMappings
  }
}
