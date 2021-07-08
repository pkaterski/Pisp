package PispLang

//import PispLang.Pisp.{Parser, pispValue, ws}
import PispLang.Parser._


object Pisp {

  println(pispValue.run("[-1.02 123 123 [1] [] -1.04]"))
  println(double.run("-1.03"))
  println(variable.run("ifa"))
  println(pispValue.run("  if    if false: true else: false   :    1    else:    2    "))
  println(pispValue.run("cond: case 1: 2 case 2: 3 else: -1"))
  println(definition.run("def \nf x:\n def\n y: 1\n def\n z: \n2 \n\ndef \ng t r: r y"))
  println(pispLambda.run("lambda x: def y: 1 x"))
  println(definition.run("def f: lambda x: def y: 1 x"))
  println(functionCall.run("f   (   x   y   if   true  :   1   else  :   2  )"))
  println(lambdaCall.run("(  lambda  x : x )   (   x   y   if   true  :   1   else  :   2  )"))
  println(pispValueAdditions.run("x @ lambda x: def y: x y"))


}
