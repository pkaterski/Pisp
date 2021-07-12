package PispLang

import PispLang.Interpreter.{State, buildIns, interpretFile, runREPL}

import java.io.FileNotFoundException


object Pisp extends App {

  val preludePath = System.getProperty("user.home") + "/.pisp/lib/prelude.pisp"
  val lib = try {
    interpretFile(preludePath, buildIns)
  } catch {
    case _: FileNotFoundException => Left(s"Can find the prelude at the path $preludePath")
    case e: Exception => Left(s"Error reading file: ${e.toString}")
  }

  lib match {
    case Right((defs, _)) =>
      run(defs)
    case Left(err) =>
      println(s"Err: The Prelude Can't Load: $err")
      run(buildIns)
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
