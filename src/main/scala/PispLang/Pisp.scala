package PispLang

import PispLang.Interpreter.{State, interpretFile, runREPL}
import PispLang.Parser._


object Pisp extends App {
  println(pispList.run("[1 2 [1 2]@head]"))

  val lib = interpretFile("./lib/prelude.pisp")

  lib match {
    case Right((defs, _)) => run(defs)
    case Left(err) => println(s"Err: The Prelude Can't Load: $err")
  }

  def run(defs: State): Unit = {
    if (args.length == 1 && args(0) == "repl") {
      println("Welcome to Pisp!")
      runREPL(defs)
    } else if (args.length == 2 && args(0) == "f") {
      interpretFile(args(1)) match {
        case Right(_) => ()
        case Left(err) => println(s"File not interpreted properly: $err")
      }
    } else {
      println("Invalid arguments")
    }
  }

}
