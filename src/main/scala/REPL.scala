import scala.io.Source

object REPL :

  @main def run() =
    def loop (str: String): Unit = 
      if (str == ":q")
        System.exit(0)
      else
        BFInterpreter(str)
        loop(Console.in.readLine)
    loop(Console.in.readLine)
