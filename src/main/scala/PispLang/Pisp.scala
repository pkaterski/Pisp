package PispLang

import PispLang.Interpreter.{State, buildIns, interpretFile, runREPL}


object Pisp extends App {

  val lib = interpretFile("./lib/prelude.pisp", buildIns)

  lib match {
    case Right((defs, _)) => run(defs)
    case Left(err) => println(s"Err: The Prelude Can't Load: $err")
  }

  def run(defs: State): Unit = {
    if (args.length == 1 && args(0) == "repl") {
      println("Welcome to Pisp!")
      runREPL(defs)
    } else if (args.length == 2 && args(0) == "f") {
      interpretFile(args(1), defs) match {
        case Right(_) => ()
        case Left(err) => println(s"File not interpreted properly: $err")
      }
    } else {
      println("Invalid arguments")
    }
  }

}
