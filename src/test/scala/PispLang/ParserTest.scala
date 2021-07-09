package PispLang

import PispLang.Parser._
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFlatSpec with Matchers {
  "int" should "parse positive numbers" in {
    int.run("14124") shouldBe Some(("",PispInt(14124)))
  }
  "int" should "parse negative numbers" in {
    int.run("-1") shouldBe Some(("",PispInt(-1)))
  }

  "double" should "parse positive numbers" in {
    double.run("14124.001") shouldBe Some(("",PispDouble(14124.001)))
  }
  "double" should "parse negative numbers" in {
    double.run("-2.124") shouldBe Some(("",PispDouble(-2.124)))
  }

  "basic definition" should "be parsed" in {
    definition.run("def x : x") shouldBe Some(("",VarDefinition("x",PispVar("x"))))
    definition.run("def x: x") shouldBe Some(("",VarDefinition("x",PispVar("x"))))
  }

  "basic if" should "be parsed" in {
    pispIf.run("if x : x else: y") shouldBe Some(("",PispIf(PispVar("x"),PispVar("x"),PispVar("y"))))
    pispIf.run("if x: x else : y") shouldBe Some(("",PispIf(PispVar("x"),PispVar("x"),PispVar("y"))))
  }

  "if without else" should "not be parsed" in {
    pispIf.run("if x: 1") shouldBe None
  }

  "cond without else" should "not be parsed" in {
    cond.run("cond: true: 1") shouldBe None
  }

  "bracketed value" should "not be parsed" in {
    pispStatement.run("(lambda x: x)") shouldBe None
  }

  "lambdas inside list" should "be parsed" in {
    pispList.run("[lambda x: x lambda x y: y]") shouldBe  Some(("",PispList(List(PispLambda(Lambda(NonEmptyList("x", Nil),List(),PispVar("x"))), PispLambda(Lambda(NonEmptyList("x", List("y")),List(),PispVar("y")))))))
  }

  "basic lambdas" should "be parsed" in {
    pispLambda.run("lambda x: x") shouldBe Some(("",Lambda(NonEmptyList("x", Nil),List(),PispVar("x"))))
    pispLambda.run("lambda  x :x") shouldBe Some(("",Lambda(NonEmptyList("x", Nil),List(),PispVar("x"))))
  }

  "basic function call" should "be parsed" in {
    functionCall.run("f(x)") shouldBe Some("",(PispFunctionCall("f",NonEmptyList(PispVar("x"), Nil))))
    functionCall.run("f   (   x    )") shouldBe Some("",(PispFunctionCall("f",NonEmptyList(PispVar("x"), Nil))))
  }

  "basic lambda call" should "be parsed" in {
    lambdaCall.run("(lambda x: x)(1)") shouldBe Some(("",PispLambdaCall(Lambda(NonEmptyList("x", Nil),List(),PispVar("x")),NonEmptyList(PispInt(1), Nil))))
    lambdaCall.run("(  lambda x  :x  )  ( 1 )") shouldBe Some(("",PispLambdaCall(Lambda(NonEmptyList("x", Nil),List(),PispVar("x")),NonEmptyList(PispInt(1), Nil))))
  }

  "basic one arg call" should "be parsed" in {
    oneArgFunctionCall.run("x @ f") shouldBe Some("",(PispOneArgFunctionCall("f",PispVar("x"))))
    oneArgFunctionCall.run("x   @   f") shouldBe Some("",(PispOneArgFunctionCall("f",PispVar("x"))))
  }

  "basic one arg lambda" should "be parsed" in {
    oneArgLambdaCall.run("x @ lambda x: x") shouldBe Some(("",PispOneArgLambdaCall(Lambda(NonEmptyList("x", Nil),List(),PispVar("x")),PispVar("x"))))
    oneArgLambdaCall.run("x@lambda    x   : x") shouldBe Some(("",PispOneArgLambdaCall(Lambda(NonEmptyList("x", Nil),List(),PispVar("x")),PispVar("x"))))
  }

  "lambda keyword" should "be separated by space for it to be parser" in {
    oneArgLambdaCall.run("x@lambdax   : x") shouldBe None
  }

  "one arg notation inside list" should "be parsed" in {
    pispList.run("[1 x@f]") shouldBe Some(("",PispList(List(PispInt(1), PispOneArgFunctionCall("f",PispVar("x"))))))
  }

}
