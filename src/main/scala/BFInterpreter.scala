import scala.collection.mutable
import scala.language.postfixOps
import scala.language.implicitConversions

class Environment (val ptr: Int, val memory: Array[Int]):

  implicit def intToByte(x: Int): Byte = x.toByte
  
  def readMemory = memory(ptr)
  def writeMemory(byte: Int) = 
    memory(ptr) = byte

  def > =
    Environment (ptr + 1, memory)

  def < = 
    Environment (ptr - 1, memory)

  def bInc =
    writeMemory(readMemory + 1)
    this

  def bDec =
    writeMemory(readMemory - 1)
    this

  def printPtr() =
    print(readMemory.toChar)

  def readByte() =
    writeMemory(Console.in.read().toByte)
       
object Environment:
  def apply (ptr: Int = 0, memory: Array[Int] = Array.fill(30000)(0)) =
    new Environment (ptr, memory)

object BFInterpreter:
  def apply (input: String) = 
    run(BFParser.parseSource(input), Environment())

  private def run(insts: Seq[BFToken], env: Environment): Environment= 
    insts match
      case Seq() => env
      case x :: xs => run (xs, exec(x, env))

  private def rep (exprs: Seq[BFToken], env: Environment): Environment =
    if env.readMemory != 0 then
      rep(exprs, run (exprs, env))
    else 
      env

  private def exec(inst: BFToken, env: Environment): Environment = 
    inst match
      case IncPtr => env >
      case DecPtr => env <
      case IncByte => env bInc
      case DecByte => env bDec
      case Print => env.printPtr(); env
      case Read => env.readByte(); env
      case Loop(exprs) => rep (exprs, env)
 
