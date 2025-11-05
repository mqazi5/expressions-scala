package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{ Success, Failure }
import TestFixtures.*
import Statement.*
import Expr.*
import behaviors.*

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

  // Helper method to verify round-trip parsing
  def verifyRoundTrip(input: String) =
    val parsed = parseRepl(input)
    val unparsed = unparse(parsed).trim
    val reparsed = parseRepl(unparsed)
    assert(parsed == reparsed)

  // Helper method to test if parsing fails
  def shouldFailToParse(input: String): Boolean =
    !parser.parseAll(parser.repl, input).successful

  // Additional Test Cases for Valid Input
  test("parser handles simple assignment: x = 5;") {
    val stmt = parseRepl("x = 5;")
    assert(stmt == Block(List(Assignment("x", Constant(5)))))
    verifyRoundTrip("x = 5;")
  }

  test("parser handles multiple assignments: x = 5; y = 7;") {
    val stmt = parseRepl("x = 5; y = 7;")
    assert(stmt == Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(7))
    )))
    verifyRoundTrip("x = 5; y = 7;")
  }

  test("parser handles complex expression: ((1 + y2) - (3 * y4)) / 5;") {
    val stmt = parseRepl("((1 + y2) - (3 * y4)) / 5;")
    assert(stmt == Block(List(
      ExpressionStmt(
        Div(
          Minus(
            Plus(Constant(1), Variable("y2")),
            Times(Constant(3), Variable("y4"))
          ),
          Constant(5)
        )
      )
    )))
    verifyRoundTrip("((1 + y2) - (3 * y4)) / 5;")
  }

  test("parser handles simple if with block: if (1) { x = 2; }") {
    val stmt = parseRepl("if (1) { x = 2; }")
    assert(stmt == Block(List(
      If(
        Constant(1),
        Block(List(Assignment("x", Constant(2)))),
        None
      )
    )))
    verifyRoundTrip("if (1) { x = 2; }")
  }

  test("parser handles if-else with blocks") {
    val stmt = parseRepl("if (1) { x = 2; } else { x = 3; }")
    assert(stmt == Block(List(
      If(
        Constant(1),
        Block(List(Assignment("x", Constant(2)))),
        Some(Block(List(Assignment("x", Constant(3)))))
      )
    )))
    verifyRoundTrip("if (1) { x = 2; } else { x = 3; }")
  }

  test("parser handles block with multiple statements") {
    val stmt = parseRepl("{ r = r + x; y = y + 1; }")
    assert(stmt == Block(List(
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Plus(Variable("y"), Constant(1)))
      ))
    )))
    verifyRoundTrip("{ r = r + x; y = y + 1; }")
  }

  test("parser handles while with multiple statements") {
    val stmt = parseRepl("while (y) { r = r + x; y = y - 1; }")
    assert(stmt == Block(List(
      While(
        Variable("y"),
        Block(List(
          Assignment("r", Plus(Variable("r"), Variable("x"))),
          Assignment("y", Minus(Variable("y"), Constant(1)))
        ))
      )
    )))
    verifyRoundTrip("while (y) { r = r + x; y = y - 1; }")
  }

  // Test Cases for Invalid Input
  test("parser rejects missing semicolon") {
    assert(shouldFailToParse("x = 5"))
  }

  test("parser rejects incomplete if") {
    assert(shouldFailToParse("if (x) x = 5;"))
  }

  test("parser rejects missing braces") {
    assert(shouldFailToParse("if (1) x = 2;"))
  }

  test("parser rejects missing parentheses") {
    assert(shouldFailToParse("if 1 { x = 2; }"))
  }

  test("parser rejects incomplete while") {
    assert(shouldFailToParse("while y { x = 1; }"))
  }

  test("parser rejects missing block") {
    assert(shouldFailToParse("while (y) x = x - 1;"))
  }

  test("parser rejects incomplete assignment") {
    assert(shouldFailToParse("x =;"))
    assert(shouldFailToParse("x = "))
  }

  test("parser rejects missing condition") {
    assert(shouldFailToParse("if () { x = 1; }"))
    assert(shouldFailToParse("while () { x = 1; }"))
  }

  test("parser rejects unmatched braces") {
    assert(shouldFailToParse("{ x = 1;"))
    assert(shouldFailToParse("x = 1; }"))
  }

  test("parser rejects invalid variable names") {
    assert(shouldFailToParse("1x = 5;"))
    assert(shouldFailToParse("$ = 5;"))
  }

end TestParser