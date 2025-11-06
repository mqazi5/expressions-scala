package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import behaviors.*
import Expr.*

class TestUnparser extends AnyFunSuite:
  
  test("unparsing simple expressions") {
    val expr = Plus(Constant(3), Times(Constant(4), Constant(5)))
    assert(unparse(expr) == "3 + (4 * 5)")
  }

  test("unparsing expressions with variables") {
    val expr = Plus(Variable("x"), Times(Variable("y"), Constant(2)))
    assert(unparse(expr) == "x + (y * 2)")
  }

  test("unparsing single statement") {
    val stmt = Assignment("x", Constant(42))
    assert(unparse(stmt) == "x = 42;")
  }

  test("unparsing if statement with proper indentation") {
    val stmt = If(
      Plus(Variable("x"), Constant(0)),
      Block(List(Assignment("y", Constant(1)))),
      None
    )
    val expected = 
      """if (x + 0) {
        |  y = 1;
        |} else {
        |}""".stripMargin
    assert(unparse(stmt).trim == expected)
  }

  test("unparsing while loop with proper indentation") {
    val stmt = While(
      Constant(0),
      Block(List(
        Assignment("x", Constant(3)),
        Assignment("y", Constant(5))
      ))
    )
    val expected =
      """while (0) {
        |  x = 3;
        |  y = 5;
        |}""".stripMargin
    assert(unparse(stmt).trim == expected)
  }

  test("unparsing nested blocks with proper indentation") {
    val stmt = Block(List(
      Assignment("x", Constant(3)),
      Block(List(
        Assignment("y", Constant(5)),
        Assignment("z", Constant(7))
      ))
    ))
    val expected =
      """{
        |  x = 3;
        |  {
        |    y = 5;
        |    z = 7;
        |  }
        |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: x = 5;") {
    val stmt = Assignment("x", Constant(5))
    assert(unparse(stmt) == "x = 5;")
  }

  test("required unparse: multiple assignments") {
    val stmt = Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(7))
    ))
    val expected = """{
      |  x = 5;
      |  y = 7;
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: ((1 + y2) - (3 * y4)) / 5;") {
    val stmt = ExpressionStmt(
      Div(
        Minus(
          Plus(Constant(1), Variable("y2")),
          Times(Constant(3), Variable("y4"))
        ),
        Constant(5)
      )
    )
    assert(unparse(stmt) == "((1 + y2) - (3 * y4)) / 5;")
  }

  test("required unparse: x = ((1 + y2) - (3 * y4)) / 5;") {
    val stmt = Assignment("x",
      Div(
        Minus(
          Plus(Constant(1), Variable("y2")),
          Times(Constant(3), Variable("y4"))
        ),
        Constant(5)
      )
    )
    assert(unparse(stmt) == "x = ((1 + y2) - (3 * y4)) / 5;")
  }

  test("required unparse: if (1) { x = 2; }") {
    val stmt = If(
      Constant(1),
      Block(List(Assignment("x", Constant(2)))),
      None
    )
    val expected = """if (1) {
      |  x = 2;
      |} else {
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: if (1) { x = 2; } else { x = 3; }") {
    val stmt = If(
      Constant(1),
      Block(List(Assignment("x", Constant(2)))),
      Some(Block(List(Assignment("x", Constant(3)))))
    )
    val expected = """if (1) {
      |  x = 2;
      |} else {
      |  x = 3;
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: { r = r + x; y = y + 1; }") {
    val stmt = Block(List(
      Assignment("r", Plus(Variable("r"), Variable("x"))),
      Assignment("y", Plus(Variable("y"), Constant(1)))
    ))
    val expected = """{
      |  r = r + x;
      |  y = y + 1;
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: if (4) { r = r + x; y = y + 1; }") {
    val stmt = If(
      Constant(4),
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Plus(Variable("y"), Constant(1)))
      )),
      None
    )
    val expected = """if (4) {
      |  r = r + x;
      |  y = y + 1;
      |} else {
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("required unparse: while (y) { r = r + x; y = y - 1; }") {
    val stmt = While(
      Variable("y"),
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Minus(Variable("y"), Constant(1)))
      ))
    )
    val expected = """while (y) {
      |  r = r + x;
      |  y = y - 1;
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("unparsing nested if-while combination with proper indentation") {
    val stmt = If(
      Variable("x"),
      Block(List(
        Assignment("y", Constant(1)),
        While(
          Variable("y"),
          Block(List(
            Assignment("z", Plus(Variable("z"), Constant(1))),
            Assignment("y", Minus(Variable("y"), Constant(1)))
          ))
        )
      )),
      None
    )
    val expected = """if (x) {
      |  y = 1;
      |  while (y) {
      |    z = z + 1;
      |    y = y - 1;
      |  }
      |} else {
      |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  test("round-trip: x = 5;") {
    val input = "x = 5;"
    val parsed = ASTBuilder.parseAll(ASTBuilder.repl, input).get
    // For round-trip, unparse the statements inside the Block
    val unparsed = parsed match
      case Block(stmts) => stmts.map(unparse(_, 0)).mkString("\n")
      case other => unparse(other)
    val reparsed = ASTBuilder.parseAll(ASTBuilder.repl, unparsed).get
    assert(parsed == reparsed)
  }

  test("round-trip: if (1) { x = 2; } else { x = 3; }") {
    val input = "if (1) { x = 2; } else { x = 3; }"
    val parsed = ASTBuilder.parseAll(ASTBuilder.repl, input).get
    val unparsed = parsed match
      case Block(stmts) => stmts.map(unparse(_, 0)).mkString("\n")
      case other => unparse(other)
    val reparsed = ASTBuilder.parseAll(ASTBuilder.repl, unparsed).get
    assert(parsed == reparsed)
  }

  test("round-trip: while (y) { r = r + x; y = y - 1; }") {
    val input = "while (y) { r = r + x; y = y - 1; }"
    val parsed = ASTBuilder.parseAll(ASTBuilder.repl, input).get
    val unparsed = parsed match
      case Block(stmts) => stmts.map(unparse(_, 0)).mkString("\n")
      case other => unparse(other)
    val reparsed = ASTBuilder.parseAll(ASTBuilder.repl, unparsed).get
    assert(parsed == reparsed)
  }

end TestUnparser
