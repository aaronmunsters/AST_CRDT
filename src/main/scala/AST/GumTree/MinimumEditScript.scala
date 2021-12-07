package AST.GumTree

import AST.Edit.AstEdit
import AST.Edit.AstEdit._
import AST.HeadedAST
import AST.Node.SchemeNode._

import scala.collection.mutable

class MinimumEditScript[Identity](var T1: HeadedAST[Identity],
                                  var T2: HeadedAST[Identity],
                                  val M: mutable.Map[Identity, Identity]) {
  var E = Seq.empty[AstEdit[Identity]]
  private var T1InOrder = Set[Identity]()
  private var T2InOrder = Set[Identity]()

  def FindPos(x_id: Identity): Int = {
    // 1. Let y = p(x) in T2 and let w be the partner of x (w in T1).
    val x = T2(x_id)
    val y_id = x.parent.get
    val y = T2(y_id).asInstanceOf[RecursiveNode[Identity]]
    // val (w_id, _) = M.find { case (_, `x_id`) => true; case _ => false }.get // TODO: determine if this is needed?
    // 2. If x is the leftmost child of y that is marked "in order", return 1.
    if (y.children.filter(T2InOrder.contains).lastOption.contains(x_id)) return 1
    // 3. Find v in T2 where v is the rightmost sibling of x that is to the left of x and is marked "in order".
    val x_pos = y.children.indexOf(x_id)
    val v_id_candidates = y.children.zipWithIndex.filter { case (identity, i) if i < y.children.indexOf(x_id) && T2InOrder.contains(identity) => true; case _ => false }
    if (x_pos == 0 || v_id_candidates.isEmpty) return 0
    val (v_id, _) = v_id_candidates.last
    // 4. Let u be the partner of v in T1.
    val (u_id, _) = M.find { case (_, `v_id`) => true; case _ => false }.get
    // 5. Suppose u is the ith child of its parent (counting from left to right) that is marked “in order”. Return i+1.
    val Some((_, u_id_pos)) = T1(T1(u_id).parent.get).asInstanceOf[RecursiveNode[Identity]].children.zipWithIndex.find{ case (`u_id`, _) => true; case _ => false }
    u_id_pos + 1
    // TODO: am I not forgetting the need to keep the root?
  }

  def subsequences[V](sequence: Seq[V]): Seq[Seq[V]] =
    for {
      start <- 0 to sequence.length
      end <- 0 to sequence.length
      if end > start
    } yield sequence.slice(start, end)

  def LongestCommonSubsequence(S1: Seq[Identity],
                               S2: Seq[Identity],
                               equal: (Identity, Identity) => Boolean): Seq[(Identity, Identity)] =
    (for {
      x1_xk <- subsequences(S1)
      y1_yk <- subsequences(S2)
      if x1_xk.length == y1_yk.length && (x1_xk zip y1_yk).forall { case (x, y) => equal(x, y) }
    } yield x1_xk zip y1_yk).sortBy(_.length*(-1)).headOption.getOrElse(Seq())

  def AlignChildren(w_identity: Identity, x_identity: Identity): Unit = {
    trait Mark
    case object OutOfOrder extends Mark
    case object InOrder extends Mark

    (T1(w_identity), T2(x_identity)) match {
      case (w: RecursiveNode[Identity], x: RecursiveNode[Identity]) =>
        var w_children: Seq[(Identity, Mark)] = w.children.map((_, OutOfOrder))
        var x_children: Seq[(Identity, Mark)] = x.children.map((_, OutOfOrder))

        // 2. Let S1 be the sequence of children of w whose partners are children of x and let S2 be the children of x whose
        //    partners are children of w.
        val S1 = w.children.filter(wc => M.exists { case (`wc`, xc) if x.contains(xc) => true; case _ => false })
        val S2 = x.children.filter(xc => M.exists { case (wc, `xc`) if w.contains(wc) => true; case _ => false })

        // 3. Define the function equal(a, b) to be true if and only if (a,b) in M.
        def equal(a: Identity, b: Identity) = M.exists { case (`a`, `b`) => true; case _ => false }

        // 4. Let S <- LCS(S1, S2, equal)
        val S = LongestCommonSubsequence(S1, S2, equal)

        // 5. For each (a,b) in S, mark nodes a and b "in order".
        for {(a, b) <- S} {
          w_children = w_children.map {
            case (`a`, _) => T1InOrder += a; (a, InOrder)
            case x => x
          }
          x_children = x_children.map {
            case (`b`, _) => T2InOrder += b; (b, InOrder)
            case x => x
          }
        }

        // For each a in S1, b in S2 such that (a,b) in M but (a,b) not in S
        for {a <- S1
             b <- S2
             if M.exists { case (`a`, `b`) => true; case _ => false } && !S.exists { case (`a`, `b`) => true; case _ => false }} {
          // (a) k <- FindPos(b)
          val k = FindPos(b)
          // (b) Append MOV(a, w, k) to E and apply MOV(a, w, k) to T1
          val move = Move(a, w_identity, k)
          E = E :+ move
          T1 = T1 perform move
          // (c) Mark a and b "in order".
          w_children = w_children.map {
            case (`a`, _) => T1InOrder += a; (a, InOrder)
            case x => x
          }
          x_children = x_children.map {
            case (`b`, _) => T2InOrder += b; (b, InOrder)
            case x => x
          }
        }
      case _ =>
    }
  }


  // INS((x, l, v), y, k) => node `x` with label `l` and value `v` inserted as the `k`th child of node `y`
  def compute(): Seq[AstEdit[Identity]] = {
    AlignChildren(T1.root.get, T2.root.get)
    // Visit the nodes of T2 in breadth-first order
    for (x_id <- T2(T2.root.get).descendants(BreadthFirst)(T2)) {
      // (a) Let x be the current node in the breadth-first search of T2 and let y = p(x). Let z be the partner of y in M.
      val x = T2(x_id)
      val y_id_option = x.parent
      val z = T1(M.map { case (k_id, v_id) => (v_id, k_id) }(y_id_option.get))
      if (!M.exists { case (_, `x_id`) => true; case _ => false }) {
        // (b) If x has no partner in M
        // i. k <- FindPos(x)
        val k = FindPos(x_id)
        // ii. Append INS((w, a, v(x)), z, k) to E, for a new identifier w.
        val add = Add(x.withoutChildren.withParent(z.id), Some(z.id), k)
        E = E :+ add
        // iii. Add (w,x) to M and apply INS((w, a, v(x)), z, k) to T1.
        M.update(x_id, x_id)
        T1 = T1 perform add
        // addition: inform the processed node is "in order"
        T1InOrder += x_id
        T2InOrder += x_id
      } else if (x != T2(T2.root.get)) {
        // (c) else if x is not a root
        // i. Let w be the partner of x in M, and let v = p(w) in T1.
        val (w_id, _) = M.find { case (_, `x_id`) => true; case _ => false }.get
        val w = T1(w_id)
        val v_id = w.parent.get
        // ii. If v(w) != v(x)
        if (!(w sameValue x)) {
          // A. Append UPD(w, v(x)) to E.
          val update = AST.Edit.UpdateValue(w.id, x.asInstanceOf[LeafNode[Identity, _]].value)
          E = E :+ update
          // B. Apply UPD(w, v(x)) to E.
          T1 = T1 perform update
        }
        // iii. If (y,v) not in M
        val y_id = y_id_option.get
        if (!M.exists { case (`v_id`, `y_id`) => true; case _ => false }) {
          // A. Let z be the partner of y in M.
          val (z_id, _) = M.find { case (_, `y_id`) => true; case _ => false }.get
          val z = T1(z_id)
          // B. k <- FindPos(x)
          val k = FindPos(x_id)
          // C. Append MOV(w, z, k) to E.
          val move = Move(w_id, z_id, k)
          E = E :+ move
          // D. Apply MOV(w, z, k) to T1.
          T1 = T1 perform move
        }
        // AlignChildren(w, x)
      }
      val (w_id, _) = M.find { case (_, `x_id`) => true; case _ => false }.get
      AlignChildren(w_id, x_id)
    }
    // 3. Do a post-order traversal of T1.
    // (a) Let w be the current node in the post-order traversal of T1.
    for (w <- T1(T1.root.get).descendants(PostOrder)(T1)) {
      // (b) If w has no partner in M then append DEL(w) to E and apply DEL(w) to T1
      if (!(M contains w)) {
        E = E :+ Delete(w)
        T1 = T1 perform Delete(w)
      }
    }
    // 4. E is a minimum cost edit script, M is a total matching, and T1 is isomorphic to T2
    E
  }
}
