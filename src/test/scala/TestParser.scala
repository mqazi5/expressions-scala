package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{ Success, Failure }
import TestFixtures.*
import Statement.*
import Expr.*

class TestParser extends AnyFunSuite:
  
  val parser = ASTBuilder

  // Helper method to parse expressions
  def parseExpr(input: String): Expr =
    parser.parseAll(parser.expr, input) match
      case parser.Success(result, _) => result
      case parser.NoSuccess(msg, _)  => throw Exception(s"Parse error: $msg")

  // Helper method to parse statements
  def parseStmt(input: String): Statement =
    parser.parseAll(parser.statement, input) match
      case parser.Success(result, _) => result
      case parser.NoSuccess(msg, _)  => throw Exception(s"Parse error: $msg")

  // Helper method to parse REPL input (multiple statements)
  def parseRepl(input: String): Statement =
    parser.parseAll(parser.repl, input) match
      case parser.Success(result, _) => result
      case parser.NoSuccess(msg, _)  => throw Exception(s"Parse error: $msg")

  // Test arithmetic expressions
  test("parser handles simple constant") {
    assert(parseExpr("5") == Constant(5))
  }

  test("parser handles simple addition") {
    assert(parseExpr("3 + 4") == Plus(Constant(3), Constant(4)))
  }

  test("parser handles nested arithmetic") {
    assert(parseExpr("2 * (3 + 4)") == 
      Times(Constant(2), Plus(Constant(3), Constant(4))))
  }

  test("parser handles unary minus") {
    assert(parseExpr("-5") == UMinus(Constant(5)))
  }

  test("parser handles variables") {
    assert(parseExpr("x") == Variable("x"))
  }

  test("parser handles complex1") {
    val expected = complex1
    val result = parseExpr("((1 + 2) * (3 + 4)) / 2 - 8")
    assert(result == expected)
  }

  // Test statements
  test("parser handles expression statement") {
    assert(parseStmt("5;") == ExpressionStmt(Constant(5)))
  }

  test("parser handles assignment") {
    assert(parseStmt("x = 5;") == 
      Assignment("x", Constant(5)))
  }

  test("parser handles simple if statement") {
    val input = """if (x > 0) { y = 1; }"""
    val expected = If(
      Plus(Variable("x"), Constant(0)),
      Block(List(Assignment("y", Constant(1)))),
      None
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles if-else statement") {
    val input = """
      if (x > 0) {
        y = 1;
      } else {
        y = -1;
      }
    """
    val expected = If(
      Plus(Variable("x"), Constant(0)),
      Block(List(Assignment("y", Constant(1)))),
      Some(Block(List(Assignment("y", UMinus(Constant(1))))))
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles while loop") {
    val input = """
      while (x > 0) {
        x = x - 1;
      }
    """
    val expected = While(
      Plus(Variable("x"), Constant(0)),
      Block(List(
        Assignment("x", Minus(Variable("x"), Constant(1)))
      ))
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles block of statements") {
    val input = """
      {
        x = 5;
        y = x * 2;
      }
    """
    val expected = Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Times(Variable("x"), Constant(2)))
    ))
    assert(parseStmt(input) == expected)
  }

  // Test REPL-style parsing (multiple statements)
  test("parser handles multiple statements") {
    val input = """
      x = 5;
      y = 10;
      while (x > 0) {
        y = y + x;
        x = x - 1;
      }
    """
    val expected = Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(10)),
      While(
        Plus(Variable("x"), Constant(0)),
        Block(List(
          Assignment("y", Plus(Variable("y"), Variable("x"))),
          Assignment("x", Minus(Variable("x"), Constant(1)))
        ))
      )
    ))
    assert(parseRepl(input) == expected)
  }

  test("parser handles factorial example") {
    val input = """
      n = 5;
      result = 1;
      while (n > 0) {
        result = result * n;
        n = n - 1;
      }
    """
    val expected = Block(List(
      Assignment("n", Constant(5)),
      Assignment("result", Constant(1)),
      While(
        Plus(Variable("n"), Constant(0)),
        Block(List(
          Assignment("result", Times(Variable("result"), Variable("n"))),
          Assignment("n", Minus(Variable("n"), Constant(1)))
        ))
      )
    ))
    assert(parseRepl(input) == expected)
  }

end TestParser