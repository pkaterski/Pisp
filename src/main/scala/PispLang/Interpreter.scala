package PispLang

import PispLang.BuildIns.Functions._
import PispLang.BuildIns.Vars.{BuildInVar, Input}
import PispLang.Parser._
import cats.data.{NonEmptyList, StateT}
import cats.data.StateT.{get, liftF, modify, set}
import cats.implicits._

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Interpreter {
  // TODO: it's possible to use a different data type here
  // Map would work but the name would be redundant, think of something
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
            s" $p1 which was evaluated from $p")
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

  def evalLambdaCall(v: PispLambdaCall): Eval[PispValue] = for {
    currDefs <- get[EitherStr, State]
    ds <- matchVarsArgs(v.lambda.vars, v.args)
    // this way combined with the searchDefinition functions it overwrites the definitions
    // but a new datatype can be used
    newDefs = ds ++ v.lambda.defs ++ currDefs
    result <- eval(v.lambda.value).run(newDefs) match {
      case Right((_, result)) => result.pure[Eval]
      case Left(error) => oops[PispValue](s"Local error occurred: $error")
    }
  } yield result

  def matchVarsArgs(vars: NonEmptyList[String], args: NonEmptyList[PispValue]): Eval[State] = for {
    ds <- (vars, args) match {
      case (NonEmptyList(v, vs), NonEmptyList(a, as)) => for {
        a1 <- eval(a)
        d = VarDefinition(v, a1)
        ds <- (NonEmptyList.fromList(vs), NonEmptyList.fromList(as)) match {
          case (Some(vs), Some(as)) => matchVarsArgs(vs, as)
          case (Some(_), None) => oops[State](s"Argument mismatch:\nvars: $vars\nargs: $args")
          case (None, Some(_)) => oops[State](s"Argument mismatch:\nvars: $vars\nargs: $args")
          case (None, None) => List().pure[Eval]
        }
      } yield d :: ds
    }
  } yield ds

  def evalFunctionCall(v: PispFunctionCall): Eval[PispValue] = for {
    state <- get[EitherStr, State]
    result <- searchDefinition(v.name, state) match {
      case Some(VarDefinition(_, PispLambda(lambda))) => evalLambdaCall(PispLambdaCall(lambda, v.args))
      case Some(VarDefinition(_, PispLambdaBuildInLink(func))) => evalBuildInFunction(func, v.args)
      case Some(VarDefinition(_, b@_)) => oops[PispValue](s"Can't call value $b with args")
      case Some(FunctionDefinition(_, lambda)) => evalLambdaCall(PispLambdaCall(lambda, v.args))
      case Some(BuildInFunctionDefinition(func)) => evalBuildInFunction(func, v.args)
      case Some(b@BuildInVarDefinition(_)) => oops[PispValue](s"Can't call build-in var $b with args")
      case None => oops[PispValue](s"Name ${v.name} is undefined")
    }
  } yield result

  def evalVar(v: PispVar): Eval[PispValue] = for {
    currDefs <- get[EitherStr, State]
    result <- searchDefinition(v.name, currDefs) match {
      case Some(VarDefinition(_, body)) => for {
        r <- eval(body)
        // TODO do it a better way
        _ <- set[EitherStr, State](VarDefinition(v.name, r) :: currDefs)
      } yield r
      case Some(FunctionDefinition(_, lambda)) => eval(PispLambda(lambda))
      case Some(BuildInFunctionDefinition(func)) => eval(PispLambdaBuildInLink(func))
      case Some(BuildInVarDefinition(variable)) => eval(PispVarBuildInLink(variable))
      case None => oops[PispValue](s"Definition for ${v.name} not found")
    }
  } yield result

  def evalBuildInFunction(f: BuildInFunction, args: NonEmptyList[PispValue]): Eval[PispValue] = f match {
    case Eq => evalBuildInEq(args).map { s: PispValue => s }
    //    case Sum => evalBuildInSum(args) // this can be implemented in prelude with add
    //    case Prod => evalBuildInProd(args) // this can be implemented in prelude with mul
    case Add => evalBuildInAdd(args)
    case Mul => evalBuildInMul(args)
    case Div => evalBuildInDiv(args)
    case Sub => evalBuildInSub(args)
    case Head => evalBuildInHead(args)
    case Tail => evalBuildInTail(args).map { s: PispValue => s }
    case Cons => evalBuildInCons(args).map { s: PispValue => s }
    case ListRef => evalBuildInListRef(args)
    case Print => evalBuildInPrint(args) // eager
    case Debug => evalBuildInDebug(args) // eager
  }

  def evalBuildInAdd(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(a, b :: Nil) => for {
      a1 <- eval(a)
      b1 <- eval(b)
      result <- (a1, b1) match {
        case (PispInt(a), PispInt(b)) => (PispInt(a + b): PispValue).pure[Eval]
        case (PispDouble(a), PispDouble(b)) => (PispDouble(a + b): PispValue).pure[Eval]
        case (PispInt(a), PispDouble(b)) => (PispDouble(a + b): PispValue).pure[Eval]
        case (PispDouble(a), PispInt(b)) => (PispDouble(a + b): PispValue).pure[Eval]
        case _ => oops[PispValue](s"Can't apply add to ${(a, b)} which eval to ${(a1, b1)}")
      }
    } yield result
    case _ => oops[PispValue](s"Can't apply add to $args")
  }

  def evalBuildInMul(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(a, b :: Nil) => for {
      a1 <- eval(a)
      b1 <- eval(b)
      result <- (a1, b1) match {
        case (PispInt(a), PispInt(b)) => (PispInt(a * b): PispValue).pure[Eval]
        case (PispDouble(a), PispDouble(b)) => (PispDouble(a * b): PispValue).pure[Eval]
        case (PispInt(a), PispDouble(b)) => (PispDouble(a * b): PispValue).pure[Eval]
        case (PispDouble(a), PispInt(b)) => (PispDouble(a * b): PispValue).pure[Eval]
        case _ => oops[PispValue](s"Can't apply mul to ${(a, b)} which eval to ${(a1, b1)}")
      }
    } yield result
    case _ => oops[PispValue](s"Can't apply mul to $args")
  }

  def evalBuildInSub(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(a, b :: Nil) => for {
      a1 <- eval(a)
      b1 <- eval(b)
      result <- (a1, b1) match {
        case (PispInt(a), PispInt(b)) => (PispInt(a - b): PispValue).pure[Eval]
        case (PispDouble(a), PispDouble(b)) => (PispDouble(a - b): PispValue).pure[Eval]
        case (PispInt(a), PispDouble(b)) => (PispDouble(a - b): PispValue).pure[Eval]
        case (PispDouble(a), PispInt(b)) => (PispDouble(a - b): PispValue).pure[Eval]
        case _ => oops[PispValue](s"Can't apply sub to ${(a, b)} which eval to ${(a1, b1)}")
      }
    } yield result
    case _ => oops[PispValue](s"Can't apply sub to $args")
  }

  def evalBuildInDiv(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(a, b :: Nil) => for {
      a1 <- eval(a)
      b1 <- eval(b)
      result <- (a1, b1) match {
        case (PispInt(a), PispInt(b)) => (PispDouble((a:Double) / (b: Double)): PispValue).pure[Eval]
        case (PispDouble(a), PispDouble(b)) => (PispDouble(a / b): PispValue).pure[Eval]
        case (PispInt(a), PispDouble(b)) => (PispDouble(a / b): PispValue).pure[Eval]
        case (PispDouble(a), PispInt(b)) => (PispDouble(a / b): PispValue).pure[Eval]
        case _ => oops[PispValue](s"Can't apply div to ${(a, b)} which eval to ${(a1, b1)}")
      }
    } yield result
    case _ => oops[PispValue](s"Can't apply div to $args")
  }

  //  def evalBuildInSum(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
  //    case NonEmptyList(PispList(Nil), Nil) => (PispInt(0): PispValue).pure[Eval]
  //    case NonEmptyList(PispList(l :: Nil), Nil) => for {
  //      l1 <- eval(l)
  //      result <- l1 match {
  //        case v@PispInt(_) => v.pure[Eval]
  //        case v@PispDouble(_) => v.pure[Eval]
  //        case v@_ => oops[PispValue](s"Sum received $l which evals to $v and is not a number")
  //      }
  //    } yield result
  //    case NonEmptyList(PispList(a :: b :: xs), Nil) => for {
  //      s <- evalBuildInAdd(NonEmptyList.of(a, b))
  //      s1 <- evalBuildInAdd(NonEmptyList.of(s, xs: _*))
  //    } yield s1
  //    case v@_ => oops[PispValue](s"Sum received an invalid arg $v")
  //  }
  //
  //  def evalBuildInProd(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
  //    case NonEmptyList(PispList(Nil), Nil) => (PispInt(1): PispValue).pure[Eval]
  //    case NonEmptyList(PispList(l :: Nil), Nil) => for {
  //      l1 <- eval(l)
  //      result <- l1 match {
  //        case v@PispInt(_) => v.pure[Eval]
  //        case v@PispDouble(_) => v.pure[Eval]
  //        case v@_ => oops[PispValue](s"Prod received $l which evals to $v and is not a number")
  //      }
  //    } yield result
  //    case NonEmptyList(PispList(a :: b :: xs), Nil) => for {
  //      s <- evalBuildInMul(NonEmptyList.of(a, b))
  //      s1 <- evalBuildInMul(NonEmptyList.of(s, xs: _*))
  //    } yield s1
  //    case v@_ => oops[PispValue](s"Prod received an invalid arg $v")
  //  }

  def evalBuildInHead(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(xs, Nil) => for {
      xs1 <- eval(xs)
      result <- xs1 match {
        case PispList(x :: _) => eval(x)
        case PispList(Nil) => oops("Head received an empty list")
        case v@_ => oops(s"Head received invalid args: $xs, after eval: $v")
      }
    } yield result
    case v@_ => oops(s"Head received invalid args: $v")
  }

  def evalBuildInTail(args: NonEmptyList[PispValue]): Eval[PispList] = args match {
    case NonEmptyList(xs, Nil) => for {
      xs1 <- eval(xs)
      result <- xs1 match {
        case PispList(_ :: xs) => PispList(xs).pure[Eval]
        case PispList(Nil) => oops("Tail received an empty list")
        case v@_ => oops(s"Tail received invalid args: $xs, after eval: $v")
      }
    } yield result
    case v@_ => oops(s"Tail received invalid args: $v")
  }

  def evalBuildInCons(args: NonEmptyList[PispValue]): Eval[PispList] = args match {
    case NonEmptyList(x, xs :: Nil) => for {
      x <- eval(x)
      xs1 <- eval(xs)
      result <- xs1 match {
        case PispList(xs) => PispList(x :: xs).pure[Eval]
        case _ => oops(s"Cons received a non-list 2nd arg $xs after eval $xs1")
      }
    } yield result
    case v@_ => oops(s"Cons received invalid args: $v")
  }

  def evalBuildInListRef(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(xs, i :: Nil) => for {
      xs1 <- eval(xs)
      i1 <- eval(i)
      result <- (xs1, i1) match {
        case (PispList(y :: _), PispInt(0)) => eval(y)
        case (PispList(Nil), PispInt(0)) => oops("ListRef is out of range")
        case (PispList(_ :: ys), PispInt(i)) if i > 0 =>
          evalBuildInListRef(NonEmptyList.of(PispList(ys), PispInt(i - 1)))
        case (PispList(_ :: ys), PispInt(i)) if i < 0 => oops("ListRef is out of range")
        case _ => oops(s"ListRef received a non-list 2nd arg $xs after eval $xs1")
      }
    } yield result
    case v@_ => oops(s"ListRef received invalid args: $v")
  }

  def evalBuildInPrint(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(x, ret :: Nil) => for {
      x <- eval(x)
      _ <- println(show(x)).pure[Eval]
      ret <- eval(ret)
    } yield ret
    case v@_ => oops(s"Print received invalid args: $v")
  }

  def evalBuildInDebug(args: NonEmptyList[PispValue]): Eval[PispValue] = args match {
    case NonEmptyList(x, Nil) => for {
      x <- eval(x)
      _ <- println(show(x)).pure[Eval]
    } yield x
    case v@_ => oops(s"Debug received invalid args: $v")
  }

  def evalBuildInEq(args: NonEmptyList[PispValue]): Eval[PispBool] = args match {
    case NonEmptyList(x, y :: Nil) => for {
      x <- eval(x)
      y <- eval(y)
      result <- (x, y) match {
        case (PispBool(a), PispBool(b)) => PispBool(a == b).pure[Eval]
        case (PispInt(a), PispInt(b)) => PispBool(a == b).pure[Eval]
        case (PispDouble(a), PispDouble(b)) => PispBool(a == b).pure[Eval]
        case (PispStr(a), PispStr(b)) => PispBool(a == b).pure[Eval]
        // no need to test if two non-empty lists are equal
        case (PispList(Nil), PispList(Nil)) => PispBool(true).pure[Eval]
        // this is kinda meaningless
        // case (PispVarBuildInLink(a), PispVarBuildInLink(b)) => PispBool(a == b).pure[Eval]
        // case (PispLambdaBuildInLink(a), PispLambdaBuildInLink(b)) => PispBool(a == b).pure[Eval]
        case _ => PispBool(false).pure[Eval]
      }
    } yield result
    case v@_ => oops(s"Eq received invalid args: $v")
  }

  def evalBuildInVar(v: BuildInVar): Eval[PispValue] = v match {
    case Input =>
      val input = readLine()
      (PispStr(input): PispValue).pure[Eval]
  }

  def eval(v: PispValue): Eval[PispValue] = v match {
    case v@PispBool(_) => (v: PispValue).pure[Eval]
    case v@PispInt(_) => (v: PispValue).pure[Eval]
    case v@PispDouble(_) => (v: PispValue).pure[Eval]
    case v@PispStr(_) => (v: PispValue).pure[Eval]
    case v@PispIf(_, _, _) => evalIf(v)
    case v@PispCond(_, _) => evalCond(v)
    case v@PispLambda(_) => (v: PispValue).pure[Eval]
    case v@PispVar(_) => evalVar(v)
    case v@PispList(_) => (v: PispValue).pure[Eval] // Lists are lazy!
    case v@PispLambdaCall(_, _) => evalLambdaCall(v)
    case PispOneArgLambdaCall(lambda, arg) => evalLambdaCall(PispLambdaCall(lambda, NonEmptyList.of(arg)))
    case v@PispFunctionCall(_, _) => evalFunctionCall(v)
    case PispOneArgFunctionCall(name, arg) => evalFunctionCall(PispFunctionCall(name, NonEmptyList.of(arg)))
    case v@PispLambdaBuildInLink(_) => (v: PispValue).pure[Eval]
    case PispVarBuildInLink(variable) => evalBuildInVar(variable)
  }

  def evalStatement(s: PispStatement): Eval[Option[PispValue]] = s match {
    case ValueStatement(v) => eval(v).map(Some(_))
    case DefinitionStatement(d) => evalDefinition(d).map(_ => None)
  }

  val buildIns: State = List(
    BuildInFunctionDefinition(Eq),
    BuildInFunctionDefinition(Add),
    BuildInFunctionDefinition(Mul),
    BuildInFunctionDefinition(Sub),
    BuildInFunctionDefinition(Div),
    BuildInFunctionDefinition(Head),
    BuildInFunctionDefinition(Tail),
    BuildInFunctionDefinition(Cons),
    BuildInFunctionDefinition(ListRef),
    BuildInFunctionDefinition(Print),
    BuildInFunctionDefinition(Debug),
    BuildInVarDefinition(Input),
  )

  def evalIfParsed(code: String): Unit = {
    val parsed = pispStatement.run(code)
    parsed match {
      case Some(("", x)) => println(evalStatement(x).run(buildIns))
      case Some((str, _)) => println(s"not parsed: $str")
      case _ => println("can't parse at all")
    }
  }

  def show(p: PispValue): String = p match {
    case PispBool(b) => b.toString
    case PispInt(i) => i.toString
    case PispDouble(d) => d.toString
    case PispStr(s) => s //s"\"$s\""
    case v@PispIf(_, _, _) => v.toString
    case v@PispCond(_, _) => v.toString
    case PispLambda(_) => "<lambda function>"
    case PispVar(n) => s"<variable: $n>"
    case PispList(l) => s"<${l.map(show)}>"
    case v@PispLambdaCall(_, _) => v.toString
    case v@PispOneArgLambdaCall(lambda, arg) => v.toString
    case v@PispFunctionCall(_, _) => v.toString
    case v@PispOneArgFunctionCall(name, arg) => v.toString
    case v@PispLambdaBuildInLink(_) => v.toString
    case v@PispVarBuildInLink(variable) => v.toString
  }

  @tailrec
  def display(d: List[Option[PispValue]]): Unit = d match {
    case Some(value) :: xs =>
      println(show(value))
      display(xs)
    case None :: xs => display(xs)
    case Nil => ()
  }

  @tailrec
  def runREPL(defs: State): Unit = {
    print("> ")
    val line = readLine()
    if (line == "exit")
      ()
    else {
      many(pispStatement).run(line) match {
        case Some(("", values)) =>
          values.map(evalStatement).sequence.run(defs) match {
            case Right((defs1, results)) =>
              display(results)
              runREPL(defs1)
            case Left(err) =>
              println(err)
              runREPL(defs)
          }
        case _ =>
          println("bad syntax")
          runREPL(defs)
      }
    }
  }

  def interpretFile(path: String): EitherStr[(State, List[Option[PispValue]])] = {
    val buffer = Source.fromFile(path)
    val code = buffer.getLines.mkString("\n")
    buffer.close
    many(pispStatement).run(code) match {
      case Some(("", values)) =>
        values.map(evalStatement).sequence.run(buildIns)
      case Some((s, _)) =>
        Left(s"file didn't parse after $s")
      case None =>
        Left("File didn't parse at all")
    }
  }


}
