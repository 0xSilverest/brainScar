import scala.util.parsing.combinator._

sealed trait BFToken

case object IncPtr extends BFToken
case object DecPtr extends BFToken
case object IncByte extends BFToken
case object DecByte extends BFToken
case object Print extends BFToken
case object Read extends BFToken
case class Loop (exprs: Seq[BFToken]) extends BFToken

object BFParser extends RegexParsers :
  
  override protected val whiteSpace = """[^\[\].,<>+-]+""".r
  
  def incPtr: Parser[IncPtr.type] = ">" ^^^ IncPtr
  def decPtr: Parser[DecPtr.type] = "<" ^^^ DecPtr
  def incByte: Parser[IncByte.type] = "+" ^^^ IncByte
  def decByte: Parser[DecByte.type] = "-" ^^^ DecByte
  def printByte: Parser[Print.type] = "." ^^^ Print
  def readByte: Parser[Read.type] = "," ^^^ Read
  def loop: Parser[Loop] = ("[" ~> rep(expr) <~ "]") ^^ Loop.apply
  def expr: Parser[BFToken] = 
    incPtr 
    | decPtr 
    | incByte 
    | decByte
    | printByte
    | readByte
    | loop
  
  def statement = rep1(expr)

  def parseSource(input: String): Either[] =
    parse(statement, input) match
      case Success (result, _) => result
      case Failure (msg, _) => throw new Exception (s"Failure : $msg")
      case Error (msg, _) => throw new RuntimeException (msg)

