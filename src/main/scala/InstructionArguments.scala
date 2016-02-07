
trait InstructionArguments {

  abstract class InstructionArgument

  case class Grx(val x: Int) extends InstructionArgument

  case class DecimalConst(val value: Int) extends InstructionArgument

  case class HexConst(val value: Int) extends InstructionArgument

  case class AddressConst(val value: String) extends InstructionArgument

  case class CharConst(val value: String) extends InstructionArgument

  abstract class Literal extends InstructionArgument

  case class DecimalConstLiteral(val value: Int) extends Literal

  case class HexConstLiteral(val value: Int) extends Literal

  case class CharConstLiteral(val value: String) extends Literal

}
