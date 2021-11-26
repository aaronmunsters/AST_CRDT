package AST.Edit
import AST.HeadedAST
import AST.Node.SchemeExpression

// Moving the current complete tree underneath a new tree could then be:
//

// TODO: determine how one could hang the current root under a different node

case class Move[Identity](child: Identity, parent: Identity, index: Int) extends AstEdit[Identity] {

  // What needs to happen when moving tree A to tree B:
  //  - node A self: parent must be updated
  //  - node B self: list of children must be updated
  //  - Parent of A: list of children must be updated

  override def perform(ast: HeadedAST[Identity]): HeadedAST[Identity] = {
    ast
  }
}
