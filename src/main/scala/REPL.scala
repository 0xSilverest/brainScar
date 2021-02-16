import scala.io.Source

object REPL :
  def loop: Unit =
    val input = Console.in.readLine
    if input == ":q" then
      System.exit(0)
    else 
      BFInterpreter.run(input)
      loop

  @main def run() =
    loop
    
