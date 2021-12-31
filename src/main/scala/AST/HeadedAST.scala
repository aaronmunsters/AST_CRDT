package AST

import AST.Edit.AstEdit
import AST.GumTree.{GumTreeAlgorithm, MinimumEditScript}
import AST.Node.SchemeNode.{LeafNode, PreOrder, RecursiveNode}
import AST.Node._

object HeadedAST {
  def empty[Identity]: HeadedAST[Identity] = HeadedAST(Map(), None)

  def withRoot[Identity](root: SchemeNode[Identity]): HeadedAST[Identity] =
    HeadedAST(Map(root.id -> root), Some(root.id))

  def computeChanges[Identity](before: HeadedAST[Identity], after: HeadedAST[Identity]): Seq[AstEdit[Identity]] = {
    val mapping = GumTreeAlgorithm(before, after).mappings(before.root.get, after.root.get)
    new MinimumEditScript(before, after, mapping).compute()
  }
}

case class HeadedAST[Identity](header: Map[Identity, SchemeNode[Identity]], root: Option[Identity]) {
  private implicit val headedAst: HeadedAST[Identity] = this

  def apply(identity: Identity): SchemeNode[Identity] = header(identity)

  def perform(edit: Edit.AstEdit[Identity]): HeadedAST[Identity] = edit.perform(this)

  def contains(identity: Identity): Boolean = header contains identity

  def hasRoot(identity: Identity): Boolean = root contains identity

  def isAncestorOf(potentialParent: Identity, potentialChild: Identity): Boolean = {
    assert(contains(potentialParent) && contains(potentialChild))
    val parentIdOption = header(potentialChild).parent
    if (parentIdOption.isEmpty) return false
    val parentId = parentIdOption.get
    if (parentId == potentialParent) return true
    isAncestorOf(potentialParent, parentId)
  }

  def toAstString(start: Option[Identity] = root): String =
    start.map(header(_).toAstString).getOrElse("")

  def toIdentifiedString(start: Option[Identity] = root): String =
    start.map(header(_).toIdentifiedString(this)).getOrElse("")

  def toPrettyAstString(indentation: Int = 4): String = {
    def nodeToString(depth: Int, nodeIdentity: Identity): String = {
      assert(contains(nodeIdentity))
      val subtree = header(nodeIdentity) match {
        case expression: RecursiveNode[Identity] =>
          if (expression.children.isEmpty) "()" else {
            val head +: tail = expression.children
            s"(${(nodeToString(0, head) +: tail.map(nodeToString(depth + indentation, _))).mkString("\n")})"
          }
        case node: LeafNode[_, _] => node.toAstString(this)
      }
      " " * depth ++ subtree
    }

    root.map(nodeToString(0, _)).getOrElse("")
  }

  def isomorphic(other: HeadedAST[Identity]): Boolean = {
    for {
      myRoot <- root
      otherRoot <- other.root
    } if (header(myRoot) isomorphic (this, other(otherRoot), other)) return true
    false
  }

  def idAt(position: Int): Option[(Identity, Int)] = {
    def findIter(nodeIdentity: Identity): Option[(Identity, Int)] =
      header(nodeIdentity) match {
        case node: LeafNode[_, _] => Some(nodeIdentity, position - node.start)
        case node: SchemeNode.RecursiveNode[Identity] if node.children.isEmpty => Some(nodeIdentity, position - node.start)
        case node: SchemeNode.RecursiveNode[Identity] =>
          node.children.map(header(_)).filter(_.start <= position).lastOption.map(_.id).flatMap(findIter)
      }
    root.flatMap(findIter)
  }

  def idAtConsidering(position: Int, source: String, uniqueIdGenerator: () => Identity): Option[(Identity, Int)] = {
    for {
      consideringTree <- Parse.Parser.parse(source, uniqueIdGenerator)
      (ident, offset) <- consideringTree.idAt(position)
      t1 <- consideringTree.root
      t2 <- root
      mapping = GumTree.GumTreeAlgorithm(consideringTree, this).mappings(t1, t2)
      currentNodeId <- mapping.get(ident)
      currentNode <- header.get(currentNodeId)
    } yield (currentNode.id, offset)
  }

  def startPos(identity: Identity, indentation: Int = 4): Int = {
    val comesBefore = root.flatMap(header.get).map(root => root.id +: root.descendants(PreOrder)).map(nodes => {
      val key_pos = nodes.indexOf(identity)
      nodes.map(n => (n, nodes.indexOf(n) < key_pos))
    }).get.toMap

    def addingOffset(depth: Int, nodeIdentity: Identity): Int = {
      if(identity == nodeIdentity) return depth
      if(!comesBefore(nodeIdentity)) return 0
      assert(contains(nodeIdentity))
      val subtree = header(nodeIdentity) match {
        case node: SchemeNode[_] if identity == node.id => 0
        case node: SchemeNode[_] if !comesBefore(node.id) => 0
        case node: LeafNode[_, _] if comesBefore(node.id) => node.toAstString(this).length
        case node: SchemeNode.RecursiveNode[_] if node.children.isEmpty => 2
        case node: SchemeNode.RecursiveNode[_] =>
          val (head: Identity) +: (tail: Seq[Identity]) = node.children
          val parentheses = if(isAncestorOf(nodeIdentity, identity)) 1 else 2
          val headLength = addingOffset(0, head)
          val tailLength = tail.map(addingOffset(depth + indentation, _)).sum
          val tailNewlines = if (tail.nonEmpty) tail.count(n => comesBefore(n) || n == identity) else 0
          parentheses + headLength + tailLength + tailNewlines
      }
      1 * depth + subtree
    }

    root
      .map(addingOffset(0, _)).getOrElse(0)
  }
}
