package AST

import AST.Edit._
import AST.Node.{SchemeExpression, SchemeIdentifier, SchemeNumber, SchemeString}
import boopickle.Default._

import java.nio.ByteBuffer

trait ReplicatedOperation[Identity, EditIdentity] {
  val editIdentity: EditIdentity
  val edit: AstEdit[Identity]
}

sealed trait SerializableAstEdit

sealed trait SerializableAst

private sealed case class RepIntOp(editIdentity: (Int, Int), edit: SerializableAstEdit)

object SerializableAstEdit {
  case class Add(tree: SerializableAst, parent: Option[Int], index: Int) extends SerializableAstEdit
  case class Delete(target: Int)                                         extends SerializableAstEdit
  case class Move(child: Int, newParent: Int, index: Int)                extends SerializableAstEdit
  case class UpdateString(target: Int, value: Array[Char])               extends SerializableAstEdit
  case class UpdateNumber(target: Int, value: Long)                      extends SerializableAstEdit

  case class AstExpression(id: Int, parent: Option[Int], children: Array[Int]) extends SerializableAst
  case class AstIdentifier(id: Int, parent: Option[Int], value: Array[Char])   extends SerializableAst
  case class AstNumber(id: Int, parent: Option[Int], value: Long)              extends SerializableAst
  case class AstString(id: Int, parent: Option[Int], value: Array[Char])       extends SerializableAst

  def astEditToSerializableEdit(astEdit: AstEdit[Int]): SerializableAstEdit = astEdit match {
    case delete: AstEdit.Delete[Int] => Delete(delete.target)
    case move: AstEdit.Move[Int] => Move(move.child, move.newParent, move.index)
    case value: AstEdit.UpdateValue[Int] => value.value match {
      case s: String => UpdateString(value.target, s.toCharArray)
      case n: Long => UpdateNumber(value.target, n)
    }
    case add: AstEdit.Add[Int] =>
      val serTree = add.tree match {
        case expression: SchemeExpression[Int] => AstExpression(expression.id, expression.parent, expression.children.toArray)
        case identifier: SchemeIdentifier[Int] => AstIdentifier(identifier.id, identifier.parent, identifier.value.toCharArray)
        case number: SchemeNumber[Int] => AstNumber(number.id, number.parent, number.value)
        case string: SchemeString[Int] => AstString(string.id, string.parent, string.value.toCharArray)
      }
      Add(serTree, add.parent, add.index)
  }

  def serializedEditToEdit(serEdit: SerializableAstEdit): AstEdit[Int] = serEdit match {
    case Delete(target) => Edit.Delete(target)
    case Move(child, newParent, index) => Edit.Move(child, newParent, index)
    case UpdateString(target, value) => Edit.UpdateValue(target, value.mkString(""))
    case UpdateNumber(target, value) => Edit.UpdateValue(target, value)
    case Add(tree, parent, index) =>
      val hydratedTree = tree match {
        case AstExpression(id, parent, children) => SchemeExpression(id, parent, children)
        case AstIdentifier(id, parent, value) => SchemeIdentifier(id, parent, value.mkString(""))
        case AstNumber(id, parent, value) => SchemeNumber(id, parent, value)
        case AstString(id, parent, value) => SchemeString(id, parent, value.mkString(""))
      }
      Edit.Add(hydratedTree, parent, index)
  }
}

case class ReplicatedStringOp(editIdentity: (Int, Int), edit: AstEdit[Int])
  extends ReplicatedOperation[Int, (Int, Int)] {

  def serialize(): ByteBuffer =
    Pickle.intoBytes(RepIntOp(editIdentity, SerializableAstEdit.astEditToSerializableEdit(edit)))

}

object ReplicatedStringOp {
  def from(v: ReplicatedOperation[Int, (Int, Int)]): ReplicatedStringOp =
    ReplicatedStringOp(v.editIdentity, v.edit)

  def deserialize(buffer: ByteBuffer): ReplicatedStringOp = {
    val RepIntOp(editIdentity, edit) = Unpickle[RepIntOp].fromBytes(buffer)
    ReplicatedStringOp(editIdentity, SerializableAstEdit.serializedEditToEdit(edit))
  }

}