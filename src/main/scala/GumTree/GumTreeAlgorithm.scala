package GumTree

import GumTree.InMemoryAst.Node._
import GumTree.PrioritySequence.PrioritySequence

import scala.collection.mutable

object GumTreeAlgorithm {
  def dice[Identity](t1: SchemeNode[Identity], t2: SchemeNode[Identity], mapping: mutable.Map[SchemeNode[Identity], SchemeNode[Identity]]): Float = {
    if (t1.descendants.size + t2.descendants.size == 0) return if (t1 isomorphic t2) 1 else 0
    (2 * t1.descendants.count(mapping.contains)) / (t1.descendants.size + t2.descendants.size)
  }

  def isomorphicDescendants[Identity](t1: SchemeNode[Identity], t2: SchemeNode[Identity]): Seq[(SchemeNode[Identity], SchemeNode[Identity])] = {
    var res = Seq[(SchemeNode[Identity], SchemeNode[Identity])]()
    val t1_desc = t1.descendants
    var t2_desc = t2.descendants
    for (t1_d <- t1_desc) {
      t2_desc.find(_ isomorphic t1_d).foreach(t2_match => {
        res = (t1_d, t2_match) +: res
        t2_desc = t2_desc.filterNot(_ == t2_match)
      })
    }
    res
  }

  // Alternations to the paper:
  // - Checking if L1/L2 queue are non-empty
  // - L1/L2 their minimal maxHeight must be greater OR EQUAL to minHeight
  // - For candidate t1 and t2 that are isomorphic with no alternate candidates are added to M
  def topDown[Identity](T1: SchemeNode[Identity],
                        T2: SchemeNode[Identity],
                        minHeight: Int,
                        L1: PrioritySequence.PrioritySequence[Identity],
                        L2: PrioritySequence.PrioritySequence[Identity],
                        A: mutable.Map[SchemeNode[Identity], SchemeNode[Identity]],
                        M: mutable.Map[SchemeNode[Identity], SchemeNode[Identity]]):
  mutable.Map[SchemeNode[Identity], SchemeNode[Identity]] = {
    L1.push(T1)
    L2.push(T2)
    while (L1.queue.nonEmpty && L2.queue.nonEmpty && Math.min(L1.peekMax.get, L2.peekMax.get) >= minHeight) {
      if (L1.peekMax.get != L2.peekMax.get) {
        if (L1.peekMax.get > L2.peekMax.get) {
          L1.pop.get.foreach(L1.open)
        } else {
          L2.pop.get.foreach(L2.open)
        }
      } else {
        val H1 = L1.pop.get
        val H2 = L2.pop.get
        for {t1 <- H1; t2 <- H2} {
          if (t1 isomorphic t2) {
            if (T2.descendants.filterNot(_ == t2).exists(_ isomorphic t1) ||
              T1.descendants.filterNot(_ == t1).exists(_ isomorphic t2)) {
              A.update(t1, t2)
            } else {
              M.update(t1, t2)
              isomorphicDescendants(t1, t2) foreach { case (from, to) => M.update(from, to) }
            }
          }
        }
        for {t1 <- H1} {
          if (!(A ++ M).keys.toSeq.contains(t1)) L1.open(t1)
        }
        for {t2 <- H2} {
          if (!(A ++ M).values.toSeq.contains(t2)) L2.open(t2)
        }
      }
    }

    val T1Parents = SchemeNode.parentMap(T1)
    val T2Parents = SchemeNode.parentMap(T2)

    var sortedA = A.toSeq.sortBy(kv => dice(T1Parents(kv._1.id), T2Parents(kv._2.id), M))
    while (sortedA.nonEmpty) {
      val (t1, t2) = sortedA.head
      sortedA = sortedA.tail
      isomorphicDescendants(t1, t2) foreach { case (from, to) => M.update(from, to) }
      sortedA = sortedA.filter { case (k, _) => k == t1 }
      sortedA = sortedA.filter { case (_, v) => v == t2 }
    }
    M
  }

  def opt[Identity](T1: SchemeNode[Identity],
                    T2: SchemeNode[Identity]):
  Map[SchemeNode[Identity], SchemeNode[Identity]] =
    Map(T1 -> T2) // TODO: replace by a serious implementation, eg. RTED

  def bottomUp[Identity](T1: SchemeNode[Identity],
                         T2: SchemeNode[Identity],
                         minDice: Float,
                         maxSize: Int,
                         M: mutable.Map[SchemeNode[Identity], SchemeNode[Identity]]):
  mutable.Map[SchemeNode[Identity], SchemeNode[Identity]] = {
    def candidate(t1: SchemeNode[Identity], M: mutable.Map[SchemeNode[Identity], SchemeNode[Identity]]): Option[SchemeNode[Identity]] = {
      T2.descendants
        .filter(_ sameLabel t1)
        .filterNot(M.values.toSeq.contains(_))
        .headOption
      // third condition from the paper is left out, "have some matching descendants"
      // TODO: could be converted to a certain coefficient
    }

    for (t1 <- T1.descendants.filterNot(M.contains).filter(_ match {
      case node: SchemeExpression[Identity] => node.subexpressions.exists(M.contains)
      case _ => true
    })) {
      candidate(t1, M).filter(dice(t1, _, M) >= minDice).foreach(t2 => {
        M.update(t1, t2)
        if (Math.max(t1.descendants.size, t2.descendants.size) < maxSize) {
          val R = opt(t1, t2)
          for ((t1, t2) <- R) {
            if (!M.exists(_ == (t1, t2)) && (t1 sameLabel t2)) {
              M.update(t1, t2)
            }
          }
        }
      })
    }
    M
  }

  def mappings[Identity](T1: SchemeNode[Identity],
                         T2: SchemeNode[Identity],
                         bottomUpMinHeight: Int = 1,
                         topDownMinDice: Float = 0,
                         topDownMaxSize: Int = 100): mutable.Map[SchemeNode[Identity], SchemeNode[Identity]] = {
    val topDownMappings = topDown(
      T1,
      T2,
      bottomUpMinHeight,
      new PrioritySequence[Identity](),
      new PrioritySequence[Identity](),
      scala.collection.mutable.Map.empty[SchemeNode[Identity], SchemeNode[Identity]],
      scala.collection.mutable.Map.empty[SchemeNode[Identity], SchemeNode[Identity]]
    )

    val withBottomUpMappings = bottomUp(
      T1,
      T2,
      topDownMinDice,
      topDownMaxSize,
      topDownMappings
    )

    withBottomUpMappings
  }
}
