import scala.util.parsing.combinator.RegexParsers

object CaslParser extends RegexParsers with CaslLines {
  override def skipWhitespace = false

  def chrExcept(cs: Char*) = elem("", ch => cs forall (ch != _))

  def label: Parser[String] = not(grx) ~> """(?!GR[0-7])[A-Z][A-Z0-9]+""".r

  def space: Parser[String] = """[\t ]+""".r

  def instCode: Parser[String] = """[A-Z]+""".r

  def grx: Parser[Grx] = ("GR0" | "GR1" | "GR2" | "GR3" | "GR4" | "GR5" | "GR6" | "GR7") ^^ { e => Grx(e.charAt(2).toInt) }

  def decConst: Parser[DecimalConst] = opt('-') ~ """\d+""".r ^^ {
    case (Some(_) ~ d) => DecimalConst(d.toInt)
    case (None ~ d) => DecimalConst(d.toInt)
  }

  def hexConst: Parser[HexConst] = elem('#') ~> """[A-F0-9]+""".r ^^ { e => HexConst(Integer.parseInt(e, 16)) }

  def strConst: Parser[CharConst] = elem('\'') ~> rep1(chrExcept('\'') ||| "''" ^^ { e => '\'' }) <~ elem('\'') ^^ { e => CharConst(e.mkString) }

  def addConst: Parser[AddressConst] = label ^^ { AddressConst }

  def decConstLit: Parser[DecimalConstLiteral] = elem('=') ~> opt('-') ~ """\d+""".r ^^ {
    case (Some(_) ~ d) => DecimalConstLiteral(-d.toInt)
    case (None ~ d) => DecimalConstLiteral(d.toInt)
  }

  def hexConstLit: Parser[HexConstLiteral] = elem('=') ~ elem('#') ~> """[A-F0-9]+""".r ^^ { e => HexConstLiteral(Integer.parseInt(e, 16)) }

  def strConstLit: Parser[CharConstLiteral] = elem('=') ~ elem('\'') ~> rep(chrExcept('\'') | "''" ^^ { e => '\'' }) <~ elem('\'') ^^ { e => CharConstLiteral(e.mkString) }

  def arg: Parser[InstructionArgument] = grx | decConst | hexConst | strConst | addConst | decConstLit | hexConstLit | strConstLit

  def operand: Parser[Seq[InstructionArgument]] = repsep(arg, elem(','))

  def comment: Parser[String] = rep1(chrExcept('\n', '\r')) ^^ { _ mkString ""}

  def instructionLineWithOperand: Parser[CaslInstructionLine] =
    opt(label) ~ space ~ instCode ~ space ~ operand ~ opt(space ~> opt(comment)) ^^ {
      case (l ~ s1 ~ ic ~ s2 ~ op ~ c) => CaslInstructionLine(l, ic, op, c.getOrElse(Option.empty))
    }

  def instructionLineWithoutOperand: Parser[CaslInstructionLine] =
    opt(label) ~ space ~ instCode ~ opt(space ~> opt(elem(';') ~> opt(comment))) ^^ {
      case (l ~ s1 ~ ic ~ Some(c)) => CaslInstructionLine(l, ic, Seq.empty[InstructionArgument], c.getOrElse(Option.empty))
      case (l ~ s1 ~ ic ~ None) => CaslInstructionLine(l, ic, Seq.empty[InstructionArgument], Option.empty)
    }

  def instructionLine: Parser[CaslInstructionLine] = instructionLineWithOperand ||| instructionLineWithoutOperand

  def annotationLine: Parser[CaslAnnotationLine] = opt(space) ~ elem(';') ~> opt(comment) ^^ {
    CaslAnnotationLine
  }

  def line: Parser[CaslLine] = positioned(annotationLine ||| instructionLine)

  def lineSeparator: Parser[String] = "\n" | "\r\n"

  def file: Parser[Seq[CaslLine]] = rep1sep(line, lineSeparator) <~ lineSeparator

  def apply(input: String): Seq[CaslLine] = parseAll(file, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) =>  {
      val errorMsg = s"${next.pos.line}:${next.pos.column} $msg"
      scala.sys.error(errorMsg)
    }
  }
}