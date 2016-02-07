import scala.util.parsing.input.Positional

trait CaslLines extends InstructionArguments {

  abstract class CaslLine extends Positional {
    def comment: Option[String]
  }

  case class CaslInstructionLine(val label: Option[String], val operator: String, val operand: Seq[InstructionArgument], val comment: Option[String]) extends CaslLine


  case class CaslAnnotationLine(val comment: Option[String]) extends CaslLine

}
