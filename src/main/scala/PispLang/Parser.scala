package PispLang

import PispLang.BuildIns.Functions.BuildInFunction
import PispLang.BuildIns.Vars.BuildInVar
import cats.data.{NonEmptyList, StateT}
import cats.implicits._

object Parser {
  type Parser[A] = StateT[Option, String, A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    some(p).map(_.toList) <+> List.empty[A].pure[Parser]

  def some[A](p: Parser[A]): Parser[NonEmptyList[A]] =
    for {
      head <- p
      tail <- many(p)
    } yield NonEmptyList.of(head, tail: _*)

  sealed trait PispValue

  case class PispBool(value: Boolean) extends PispValue

  case class PispInt(value: Int) extends PispValue

  case class PispDouble(value: Double) extends PispValue

  case class PispStr(value: String) extends PispValue

  case class PispList(value: List[PispValue]) extends PispValue

  case class PispVar(name: String) extends PispValue

  case class PispVarBuildInLink(variable: BuildInVar) extends PispValue

  case class PispLambda(lambda: Lambda) extends PispValue

  case class PispLambdaBuildInLink(func: BuildInFunction) extends PispValue

  case class PispIf(predicate: PispValue, tVal: PispValue, fVal: PispValue) extends PispValue

  case class PispCond(testPairs: List[(PispValue, PispValue)], otherwise: PispValue) extends PispValue

  case class PispFunctionCall(name: String, args: NonEmptyList[PispValue]) extends PispValue

  case class PispOneArgFunctionCall(name: String, arg: PispValue) extends PispValue

  case class PispOneArgLambdaCall(lambda: Lambda, arg: PispValue) extends PispValue

  case class PispLambdaCall(lambda: Lambda, args: NonEmptyList[PispValue]) extends PispValue

  case class Lambda(vars: NonEmptyList[String], defs: List[Definition], value: PispValue)

  sealed trait PispStatement

  case class ValueStatement(value: PispValue) extends PispStatement

  case class DefinitionStatement(definition: Definition) extends PispStatement

  sealed trait Definition

  case class FunctionDefinition(name: String, body: Lambda) extends Definition

  case class VarDefinition(name: String, body: PispValue) extends Definition

  case class BuildInFunctionDefinition(func: BuildInFunction) extends Definition

  case class BuildInVarDefinition(variable: BuildInVar) extends Definition


  //  val item: Parser[Char] = for {
  //    input <- get[Option, String]
  //    _ <- if (input.nonEmpty)
  //      modify[Option, String](_.tail)
  //    else
  //      ().raiseError[Parser, Nothing]
  //  } yield input.head
  val item: Parser[Char] = StateT[Option, String, Char](str => for {
    x <- str.headOption
    xs = str.drop(1)
  } yield (xs, x))

  def sat(p: Char => Boolean): Parser[Char] =
    for {
      c <- item
      _ <- if (p(c)) c.pure[Parser]
      else ().raiseError[Parser, Nothing]
    } yield c

  val digit: Parser[Char] = sat(_.isDigit)

  val lower: Parser[Char] = sat(_.isLower)

  val upper: Parser[Char] = sat(_.isUpper)

  val letter: Parser[Char] = sat(_.isLetter)

  val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)

  def char(c: Char): Parser[Char] = sat(_ == c)

  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)

  val bool: Parser[PispValue] = {
    val trueP: Parser[PispValue] = string("true").map(_ => PispBool(true))
    val falseP: Parser[PispValue] = string("false").map(_ => PispBool(false))
    trueP <+> falseP
  }

  val digits: Parser[String] = some(digit).map(_.toList.mkString)
  val negDigits: Parser[String] = for {
    s <- char('-')
    ds <- digits
  } yield s + ds

  val int: Parser[PispValue] = for {
    ds <- digits <+> negDigits
  } yield PispInt(ds.toInt)

  val double: Parser[PispValue] = for {
    n <- negDigits <+> digits
    _ <- char('.')
    m <- digits
  } yield PispDouble((n + '.' + m).toDouble)

  val pispString: Parser[PispValue] = for {
    _ <- char('"')
    x <- many(sat(_ != '"')).map(_.mkString)
    _ <- char('"')
  } yield PispStr(x)

  val ws: Parser[String] = many(sat(s => s.isSpaceChar || s == '\t' || s == '\n')).map(_.mkString)
  val wsSome: Parser[String] = some(sat(s => s.isSpaceChar || s == '\t' || s == '\n')).map(_.toList.mkString)

  lazy val pispList: Parser[PispValue] = for {
    _ <- char('[')
    xs <- many(pispValueAdditions)
    _ <- char(']')
  } yield PispList(xs)

  val comment: Parser[String] = for {
    _ <- ws
    _ <- char('#')
    c <- many(sat(_ != '\n')).map(_.mkString)
  } yield c

  val comments: Parser[List[String]] = many(comment)

  val keywords = List("def", "lambda", "if", "else", "cond", "case")

  val variableString: Parser[String] = for {
    x <- letter
    ys <- many(alphaNum).map(_.mkString)
    s = x + ys
    s1 <-
      if (!keywords.contains(s))
        s.pure[Parser]
      else
        ().raiseError[Parser, Nothing]
  } yield s1

  val variable: Parser[PispValue] = variableString.map(PispVar)

  val pispIf: Parser[PispValue] = for {
    _ <- string("if")
    _ <- wsSome
    p <- pispValue
    _ <- char(':')
    t <- pispValue
    _ <- string("else")
    _ <- ws
    _ <- char(':')
    f <- pispValue
  } yield PispIf(p, t, f)

  val cond: Parser[PispValue] = for {
    _ <- string("cond")
    _ <- ws
    _ <- char(':')
    _ <- ws
    cs <- many(for {
      _ <- string("case")
      p <- pispValue
      _ <- char(':')
      v <- pispValue
    } yield (p, v))
    _ <- string("else")
    _ <- ws
    _ <- char(':')
    otherwise <- pispValue
  } yield PispCond(cs, otherwise)

  lazy val definition: Parser[Definition] = for {
    _ <- string("def")
    _ <- wsSome
    name <- variableString
    vars <- many(ws *> variableString)
    _ <- ws *> char(':')
    d <- NonEmptyList.fromList(vars) match {
      case Some(vars) => for {
        defs <- many(ws *> definition)
        value <- pispValue
        d <- FunctionDefinition(name, Lambda(vars, defs, value)).pure[Parser]
      } yield d
      case None => for {
        value <- pispValue
        d <- VarDefinition(name, value).pure[Parser]
      } yield d
    }
  } yield d

  //  val statement: Parser[PispStatement] = definition.map(DefinitionStatement)

  val pispLambda: Parser[Lambda] = for {
    _ <- string("lambda")
    _ <- wsSome
    vars <- some(ws *> variableString)
    _ <- ws *> char(':')
    defs <- many(ws *> definition)
    value <- pispValue
  } yield Lambda(vars, defs, value)

  val argsCall: Parser[NonEmptyList[PispValue]] = for {
    _ <- char('(')
    args <- some(pispValueAdditions)
    _ <- char(')')
  } yield args

  val functionCall: Parser[PispValue] = for {
    name <- variableString
    args <- ws *> argsCall
  } yield PispFunctionCall(name, args)

  val lambdaCall: Parser[PispValue] = for {
    _ <- char('(')
    lambda <- ws *> pispLambda
    _ <- char(')')
    args <- ws *> argsCall
  } yield PispLambdaCall(lambda, args)

  val oneArgFunctionCall: Parser[PispValue] = for {
    arg <- pispValue
    _ <- char('@') <* ws
    name <- variableString
  } yield PispOneArgFunctionCall(name, arg)

  val oneArgLambdaCall: Parser[PispValue] = for {
    arg <- pispValue
    _ <- char('@') <* ws
    lambda <- pispLambda
  } yield PispOneArgLambdaCall(lambda, arg)

  lazy val pispValue: Parser[PispValue] = comments *> ws *> List(
    bool,
    double,
    int,
    pispString,
    pispList,
    pispIf,
    cond,
    pispLambda.map(PispLambda): Parser[PispValue],
    functionCall,
    lambdaCall,
    variable,
  ).reduceLeft(_ <+> _) <* ws

  lazy val pispValueAdditions: Parser[PispValue] = comments *> ws *> List(
    oneArgFunctionCall,
    oneArgLambdaCall,
    pispValue,
  ).reduceLeft(_ <+> _) <* ws

  lazy val pispStatement: Parser[PispStatement] = comments *> ws *> List(
    definition.map(DefinitionStatement): Parser[PispStatement],
    pispValueAdditions.map(ValueStatement): Parser[PispStatement],
  ).reduceLeft(_ <+> _) <* ws <* comments

}
