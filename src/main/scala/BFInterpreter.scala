import scala.collection.mutable
import scala.language.postfixOps
import scala.language.implicitConversions

class Environment /*(val ptr: Int = 0, val memory: Array[Byte] = Array.fill(65534)(0))*/ :
 
  var ptr: Int = 0
  val memory: Array[Byte] = Array.fill(65534)(0)

  implicit def intToByte(x: Int): Byte = x.toByte
  
  def readMemory = memory(ptr)
  def writeMemory(byte: Byte) = 
    memory(ptr) = byte

  def > = ptr += 1

  def < = ptr -= 1

  def bInc =
    writeMemory(readMemory + 1) 

  def bDec =
    writeMemory(readMemory - 1)

  def printPtr() =
    print(readMemory.toChar)

  def readByte() =
    writeMemory(Console.in.read().toByte)

  def run(input: String) = 
    val insts = BFParser.parseSource(input)
    insts foreach exec

  def exec(inst: BFToken): Unit = 
    inst match
      case IncPtr => >
      case DecPtr => <
      case IncByte => bInc
      case DecByte => bDec
      case Print => printPtr()
      case Read => readByte()
      case Loop(exprs) =>
        while (memory(ptr) != 0)
          exprs.foreach (exec)
    
object BFInterpreter:
  def run(input: String) = new Environment().run(input)

  @main def test2() =
    run("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
