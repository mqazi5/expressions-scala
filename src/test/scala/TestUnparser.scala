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

  test("unparsing complex expressions with proper parentheses") {
    val expr = Plus(Plus(UMinus(Constant(3)), Constant(4)), Times(Constant(5), Constant(6)))
    assert(unparse(expr) == "((-3 + 4) + (5 * 6))")
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

  test("unparsing the example from prompt") {
    val stmt = Block(List(
      If(
        Plus(Plus(UMinus(Constant(3)), Constant(4)), Times(Constant(5), Constant(6))),
        Block(List(
          While(
            Constant(0),
            Block(List(
              Assignment("x", Constant(3)),
              Assignment("y", Constant(5)),
              Block(List(
                Assignment("xy", Constant(88))
              ))
            ))
          )
        )),
        Some(Block(List()))
      )
    ))
    val expected =
      """{
        |  if (((-3 + 4) + (5 * 6))) {
        |    while (0) {
        |      x = 3;
        |      y = 5;
        |      {
        |        xy = 88;
        |      }
        |    }
        |  } else {
        |  }
        |}""".stripMargin
    assert(unparse(stmt) == expected)
  }

  // Additional test cases for unparsing expressions and statements
  test("unparsing single assignment") {
    val stmt = Assignment("x", Constant(5))
    assert(unparse(stmt) == "x = 5;")
  }

  test("unparsing multiple assignments") {
    val stmt = Block(List(
      Assignment("x", Constant(5)),
      Assignment("y", Constant(7))
    ))
    assert(unparse(stmt).trim == """{
      |  x = 5;
      |  y = 7;
      |}""".stripMargin)
  }

  test("unparsing complex expression with multiple operators") {
    val expr = Div(
      Minus(
        Plus(Constant(1), Variable("y2")),
        Times(Constant(3), Variable("y4"))
      ),
      Constant(5)
    )
    assert(unparse(expr) == "((1 + y2) - (3 * y4)) / 5")
  }

  test("unparsing assignment with complex expression") {
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

  test("unparsing simple if with block") {
    val stmt = If(
      Constant(1),
      Block(List(Assignment("x", Constant(2)))),
      None
    )
    assert(unparse(stmt).trim == """if (1) {
      |  x = 2;
      |} else {
      |}""".stripMargin)
  }

  test("unparsing if-else with blocks") {
    val stmt = If(
      Constant(1),
      Block(List(Assignment("x", Constant(2)))),
      Some(Block(List(Assignment("x", Constant(3)))))
    )
    assert(unparse(stmt).trim == """if (1) {
      |  x = 2;
      |} else {
      |  x = 3;
      |}""".stripMargin)
  }

  test("unparsing block with multiple statements and proper indentation") {
    val stmt = Block(List(
      Assignment("r", Plus(Variable("r"), Variable("x"))),
      Assignment("y", Plus(Variable("y"), Constant(1)))
    ))
    assert(unparse(stmt).trim == """{
      |  r = r + x;
      |  y = y + 1;
      |}""".stripMargin)
  }

  test("unparsing if with multiple statements in block") {
    val stmt = If(
      Constant(4),
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Plus(Variable("y"), Constant(1)))
      )),
      None
    )
    assert(unparse(stmt).trim == """if (4) {
      |  r = r + x;
      |  y = y + 1;
      |} else {
      |}""".stripMargin)
  }

  test("unparsing while with multiple statements and proper indentation") {
    val stmt = While(
      Variable("y"),
      Block(List(
        Assignment("r", Plus(Variable("r"), Variable("x"))),
        Assignment("y", Minus(Variable("y"), Constant(1)))
      ))
    )
    assert(unparse(stmt).trim == """while (y) {
      |  r = r + x;
      |  y = y - 1;
      |}""".stripMargin)
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
    assert(unparse(stmt).trim == """if (x) {
      |  y = 1;
      |  while (y) {
      |    z = z + 1;
      |    y = y - 1;
      |  }
      |} else {
      |}""".stripMargin)
  }

end TestUnparser