package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import behaviors.*
import Expr.*
import Statement.*

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

end TestUnparser