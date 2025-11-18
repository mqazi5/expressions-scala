package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{ Success, Failure }
import TestFixtures.*
import Expr.*
import behaviors.*

class TestParser extends AnyFunSuite:
  
  val parser = ASTBuilder

  // Helper method to parse expressions
  def parseExpr(input: String): Expr =
    parser.parseAll(parser.expr, input) match
      case parser.Success(result, _) => result
      case parser.NoSuccess(msg, next)  => throw Exception(s"Parse error: $msg")

  // Helper method to parse statements (single statement)
  def parseStmt(input: String): Expr =
    parser.parseAll(parser.statement, input) match
      case parser.Success(result, _) => result
      case parser.NoSuccess(msg, _)  => throw Exception(s"Parse error: $msg")

  // Helper method to parse REPL input (multiple expressions)
  def parseRepl(input: String): Expr =
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
    val result = parseExpr(complex1string)
    assert(result == expected)
  }

  // Test statements
  test("parser handles expression statement") {
    assert(parseStmt("5;") == ExpressionStmt(Constant(5)))
  }

  test("parser handles assignment") {
    assert(parseStmt("x = 5;") == Assignment("x", Constant(5)))
  }

  test("parser handles simple if statement") {
    val input = """if (x) { y = 1; }"""
    val expected = If(
      Variable("x"),
      Block(List(Assignment("y", Constant(1)))),
      None
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles if-else statement") {
    val input = """
      if (x) {
        y = 1;
      } else {
        y = -1;
      }
    """
    val expected = If(
      Variable("x"),
      Block(List(Assignment("y", Constant(1)))),
      Some(Block(List(Assignment("y", UMinus(Constant(1))))))
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles while loop") {
    val input = """
      while (x) {
        x = x - 1;
      }
    """
    val expected = While(
      Variable("x"),
      Block(List(
        Assignment("x", Minus(Variable("x"), Constant(1)))
      ))
    )
    assert(parseStmt(input) == expected)
  }

  test("parser handles block of expressions") {
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

  // Test REPL-style parsing (multiple expressions)
  test("parser handles multiple expressions") {
    val input = """
      x = 5;
      y = 10;
      while (x) {
        y = y + x;
        x = x - 1;
      }
    """
    val expected = Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(10)),
      While(
        Variable("x"),
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
      while (n) {
        result = result * n;
        n = n - 1;
      }
    """
    val expected = Block(List(
      Assignment("n", Constant(5)),
      Assignment("result", Constant(1)),
      While(
        Variable("n"),
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

  test("required: x = 5;") {
    val stmt = parseRepl("x = 5;")
    assert(stmt == Block(List(Assignment("x", Constant(5)))))
  }

  test("required: x = 5; y = 7;") {
    val stmt = parseRepl("x = 5; y = 7;")
    assert(stmt == Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(7))
    )))
  }

  test("required: ((1 + y2) - (3 * y4)) / 5;") {
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
  }

  test("required: x = ((1 + y2) - (3 * y4)) / 5;") {
    val stmt = parseRepl("x = ((1 + y2) - (3 * y4)) / 5;")
    assert(stmt == Block(List(
      Assignment("x",
        Div(
          Minus(
            Plus(Constant(1), Variable("y2")),
            Times(Constant(3), Variable("y4"))
          ),
          Constant(5)
        )
      )
    )))
  }

  test("required: if (1) { x = 2; }") {
    val stmt = parseRepl("if (1) { x = 2; }")
    assert(stmt == Block(List(
      If(
        Constant(1),
        Block(List(Assignment("x", Constant(2)))),
        None
      )
    )))
  }

  test("required: if (1) { x = 2; } else { x = 3; }") {
    val stmt = parseRepl("if (1) { x = 2; } else { x = 3; }")
    assert(stmt == Block(List(
      If(
        Constant(1),
        Block(List(Assignment("x", Constant(2)))),
        Some(Block(List(Assignment("x", Constant(3)))))
      )
    )))
  }

  test("required: { r = r + x; y = y + 1; }") {
    val stmt = parseRepl("{ r = r + x; y = y + 1; }")
    assert(stmt == Block(List(
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Plus(Variable("y"), Constant(1)))
      ))
    )))
  }

  test("required: if (4) { r = r + x; y = y + 1; }") {
    val stmt = parseRepl("if (4) { r = r + x; y = y + 1; }")
    assert(stmt == Block(List(
      If(
        Constant(4),
        Block(List(
          Assignment("r", Plus(Variable("r"), Variable("x"))),
          Assignment("y", Plus(Variable("y"), Constant(1)))
        )),
        None
      )
    )))
  }

  test("required: while (y) { r = r + x; y = y - 1; }") {
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
  }

  test("required: while (y) { r = r + x; y = y - 1; } (with spaces)") {
    val stmt = parseRepl("while (y) { r = r + x ; y = y - 1 ;}")
    assert(stmt == Block(List(
      While(
        Variable("y"),
        Block(List(
          Assignment("r", Plus(Variable("r"), Variable("x"))),
          Assignment("y", Minus(Variable("y"), Constant(1)))
        ))
      )
    )))
  }

  test("invalid: x = 5 (missing semicolon)") {
    assert(shouldFailToParse("x = 5"))
  }

  test("invalid: x = ; (missing expression)") {
    assert(shouldFailToParse("x = ;"))
  }

  test("invalid: = 5; (missing variable)") {
    assert(shouldFailToParse("= 5;"))
  }

  test("invalid: if (1) x = 2; (missing braces)") {
    assert(shouldFailToParse("if (1) x = 2;"))
  }

  test("invalid: if 1 { x = 2; } (missing parentheses)") {
    assert(shouldFailToParse("if 1 { x = 2; }"))
  }

  test("invalid: if () { x = 2; } (missing condition)") {
    assert(shouldFailToParse("if () { x = 2; }"))
  }

  test("invalid: if (1) { x = 2 } (missing semicolon in block)") {
    assert(shouldFailToParse("if (1) { x = 2 }"))
  }

  test("invalid: if (1) { x = 2; } else x = 3; (else missing braces)") {
    assert(shouldFailToParse("if (1) { x = 2; } else x = 3;"))
  }

  test("invalid: while (y) r = r + x; (missing braces)") {
    assert(shouldFailToParse("while (y) r = r + x;"))
  }

  test("invalid: while y { r = r + x; } (missing parentheses)") {
    assert(shouldFailToParse("while y { r = r + x; }"))
  }

  test("invalid: while () { x = 1; } (missing condition)") {
    assert(shouldFailToParse("while () { x = 1; }"))
  }

  test("invalid: { x = 1; (unmatched opening brace)") {
    assert(shouldFailToParse("{ x = 1;"))
  }

  test("invalid: x = 1; } (unmatched closing brace)") {
    assert(shouldFailToParse("x = 1; }"))
  }

  test("invalid: 1x = 5; (invalid variable name starting with digit)") {
    assert(shouldFailToParse("1x = 5;"))
  }

  test("invalid: $ = 5; (invalid variable name with special character)") {
    assert(shouldFailToParse("$ = 5;"))
  }

  test("invalid: x = (1 + 2; (unmatched parenthesis)") {
    assert(shouldFailToParse("x = (1 + 2;"))
  }

  test("invalid: x = 1 + ; (incomplete expression)") {
    assert(shouldFailToParse("x = 1 + ;"))
  }

end TestParser
