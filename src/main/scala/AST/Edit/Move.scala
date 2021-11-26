package AST.Edit
import AST.HeadedAST

case class Move[Identity](child: Identity, parent: Identity, index: Int) extends AstEdit[Identity] {

  // What needs to happen when moving tree A to tree B:
  //  - node A self: parent must be updated
  //  - node B self: list of children must be updated
  //  - Parent of A: list of children must be updated

  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    assert(ast contains child)
    assert(ast contains parent)
    assert(! (ast hasRoot child))
    assert(! (ast.isAncestorOf(child, parent)))

    val childTree = ast.header(child)
    val parentTree = ast.header(parent)

    ast
  }
}
