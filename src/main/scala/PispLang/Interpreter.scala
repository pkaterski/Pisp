package PispLang

import PispLang.Parser._
import cats.data.StateT
import cats.data.StateT.{get, liftF, modify}
import cats.implicits._

import scala.annotation.tailrec

object Interpreter extends App {
  type State = List[Definition]
  type EitherStr[A] = Either[String, A]
  type Eval[A] = StateT[EitherStr, State, A]
  //  type Eval[A] = StateT[({type t[T] = Either[String, T]})#t, State, A]

  def oops[A](msg: String): Eval[A] = liftF[EitherStr, State, A](Left(msg))

  @tailrec
  def searchDefinition(name: String, s: State): Option[Definition] = {
    // explicitly check all cases and NO wildcards, so the compiler can warn if a definition type is missing
    s match {
      case Nil => None
      case x@FunctionDefinition(someName, _) :: _ if someName == name => Some(x.head)
      case FunctionDefinition(_, _) :: xs => searchDefinition(name, xs)
      case x@VarDefinition(someName, _) :: _ if someName == name => Some(x.head)
      case VarDefinition(_, _) :: xs => searchDefinition(name, xs)
      case x@BuildInVarDefinition(d) :: _ if d.getName == name => Some(x.head)
      case BuildInVarDefinition(_) :: xs => searchDefinition(name, xs)
      case x@BuildInFunctionDefinition(d) :: _ if d.getName == name => Some(x.head)
      case BuildInFunctionDefinition(_) :: xs => searchDefinition(name, xs)
    }
  }

  def isDefined(name: String, s: State): Boolean = {
    searchDefinition(name, s) match {
      case Some(_) => true
      case None => false
    }
  }

  def evalIf(v: PispIf): Eval[PispValue] = for {
    p <- eval(v.predicate)
    result <- p match {
      case PispBool(true) => eval(v.tVal)
      case PispBool(false) => eval(v.fVal)
      case _ => oops("Non-boolean predicate in if statement: " + p.toString)
    }
  } yield result

  def evalCond(v: PispCond): Eval[PispValue] = for {
    result <- v.testPairs match {
      case Nil => eval(v.otherwise)
      case (p, t) :: xs => for {
        p1 <- eval(p)
        result <- p1 match {
          case PispBool(true) => eval(t)
          case PispBool(false) => evalCond(PispCond(xs, v.otherwise))
          case _ => oops[PispValue](s"Non-boolean predicate in cond statement:" +
            s" $p1 which was evaluated from $p" )
        }
      } yield result
    }
  } yield result

  def evalDefinition(v: Definition): Eval[Unit] = for {
    name <- v match {
      case FunctionDefinition(name, _) => name.pure[Eval]
      case VarDefinition(name, _) => name.pure[Eval]
      case BuildInFunctionDefinition(_) => oops[String]("Can't eval build-in function definition")
      case BuildInVarDefinition(_) => oops[String]("Can't eval build-in var definition")
    }
    s <- get[EitherStr, State]
    _ <- if (!isDefined(name, s))
      modify[EitherStr, State](state => v :: state)
    else
      oops[Unit](s"Definition for $name already exists")
  } yield ()

//  def evalLambdaCall(v: PispLambdaCall): Eval[PispValue] = for {
//    s <- get[EitherStr, State]
//    _ <- v.args
//  } yield ???
//
//  def matchVarsArgs(vars: List[String], args: List[PispValue]): Eval[State] = for {
//    ds <- (vars, args) match {
//      case (v :: vs, a :: as) => for {
//        a1 <- eval(a)
//        d =
//      }
//      case _ => oops[State](s"Argument mismatch: vars: $vars\nargs: $args")
//    }
//  } yield ds

  def eval(v: PispValue): Eval[PispValue] = v match {
    case v@PispBool(_) => (v: PispValue).pure[Eval]
    case v@PispInt(_) => (v: PispValue).pure[Eval]
    case v@PispDouble(_) => (v: PispValue).pure[Eval]
    case v@PispStr(_) => (v: PispValue).pure[Eval]
    case v@PispIf(_, _, _) => evalIf(v)
    case v@PispCond(_, _) => evalCond(v)
    case _ => ???
  }

  def evalStatement(s: PispStatement): Eval[Option[PispValue]] = s match {
    case ValueStatement(v) => eval(v).map(Some(_))
    case DefinitionStatement(d) => evalDefinition(d).map(_ => None)
  }

  def evalIfParsed(code: String): Unit = {
    val parsed = pispStatement.run(code)
    parsed match {
      case Some(("", x)) => println(evalStatement(x).run(List()))
      case Some((str, _)) => println(s"not parsed: $str")
      case _ => println("can't parse at all")
    }
  }

  evalIfParsed(" if if false : true else : true : 1 else : 2")
  evalIfParsed(" cond: case false: 1 case if false: false else: true : 2 else : 3")
  evalIfParsed(" def x z: def y: 1 y")

}
