package PispLang

import PispLang.Interpreter._
import PispLang.Parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterTest extends AnyFlatSpec with Matchers {
  "prelude" should "not have any errors" in {
    interpretFile("./lib/prelude.pisp", buildIns).isRight shouldBe true
  }
  "basic build-in Add" should "work for ints" in {
    pispStatement.run("add(1 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispInt(3))))
  }
  "basic build-in Add" should "work for doubles" in {
    pispStatement.run("add(1.0 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(3))))
    pispStatement.run("add(1 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(3))))
    pispStatement.run("add(1.0 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(3))))
  }
  "basic build-in Sub" should "work for ints" in {
    pispStatement.run("sub(1 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispInt(-1))))
  }
  "basic build-in Sub" should "work for doubles" in {
    pispStatement.run("sub(1.0 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(-1))))
    pispStatement.run("sub(1 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(-1))))
    pispStatement.run("sub(1.0 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(-1))))
  }
  "basic build-in Mul" should "work for ints" in {
    pispStatement.run("mul(3 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispInt(6))))
  }
  "basic build-in Mul" should "work for doubles" in {
    pispStatement.run("mul(3.0 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(6))))
    pispStatement.run("mul(3 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(6))))
    pispStatement.run("mul(3.0 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(6))))
  }
  "basic build-in Div" should "work for all numbers" in {
    pispStatement.run("div(1 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(.5))))
    pispStatement.run("div(1.0 2)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(.5))))
    pispStatement.run("div(1 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(.5))))
    pispStatement.run("div(1.0 2.0)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispDouble(.5))))
  }

  "basic build-in listRef" should "work" in {
    pispStatement.run("listRef([1 2 3] 1)").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispInt(2))))
  }
  "basic build-in Head" should "work" in {
    pispStatement.run("head([1 2 3])").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispInt(1))))
  }
  "basic build-in Head" should "go out of bounds on empty list" in {
    pispStatement.run("head([])").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Left("Head received an empty list"))
  }
  "basic build-in Tail" should "work" in {
    pispStatement.run("tail([1 2 3])").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispList(List(PispInt(2), PispInt(3))))))
  }
  "basic build-in Tail" should "go out of bounds on empty list" in {
    pispStatement.run("tail([])").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Left("Tail received an empty list"))
  }
  "basic build-in Cons" should "work" in {
    pispStatement.run("cons(1 [2 3])").map(s => evalStatement(s._2).run(buildIns).map(_._2)) shouldBe
      Some(Right(Some(PispList(List(PispInt(1), PispInt(2), PispInt(3))))))
  }
}
