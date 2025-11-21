package edu.luc.cs.laufer.cs371.expressions

import util.{ Success, Failure }
import org.scalatest.funsuite.AnyFunSuite

import behaviors.*
import TestFixtures.*

object Main:
  def main(args: Array[String]): Unit =
    // Process expressions
    processExpr("p", complex1)
    processExpr("q", complex2)
    processExpr("f", bad)

    // Process statements
    val factorial = """
      n = 5;
      result = 1;
      while (n > 0) {
        result = result * n;
        n = n - 1;
      }
    """
    processStatements("Factorial Program", factorial)

  def processExpr(n: String, e: Expr): Unit =
    println(s"$n = $e")
    println(s"evaluate($n) = ${evaluate(e)}")
    println(s"size($n) = ${size(e)}")
    println(s"height($n) = ${height(e)}")

  def processStatements(name: String, input: String): Unit =
    println(s"\nProcessing $name:")
    println(s"Input:\n$input")
    ASTBuilder.parseAll(ASTBuilder.repl, input) match
      case ASTBuilder.Success(result, _) =>
        println(s"AST: $result")
      case ASTBuilder.NoSuccess(msg, _) =>
        println(s"Parse error: $msg")

end Main
// need to change tests for 3b functionalist value based
//getting very werid errors from this part
class Test extends AnyFunSuite:
  test("evaluate(p)") {
    evaluate(complex1) match
      case Success(Num(v)) => assert(v == -1)
      case other           => fail(s"Expected Success(Num(-1)), got $other")
  }

  test("size(p)") { assert(size(complex1) == 9) }
  test("height(p)") { assert(height(complex1) == 4) }

  test("evaluate(q)") {
    evaluate(complex2) match
      case Success(Num(v)) => assert(v == 0)
      case other           => fail(s"Expected Success(Num(0)), got $other")
  }

  test("size(q)") { assert(size(complex2) == 10) }
  test("height(q)") { assert(height(complex2) == 5) }

  test("evaluate(bad)") { assert(evaluate(bad).isFailure) }
end Test

