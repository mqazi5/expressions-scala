package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import behaviors.*
import Statement.*
import Expr.*
import org.json4s.native.JsonMethods.*

class TestToJson extends AnyFunSuite:
  
  // Helper method to pretty print JSON
  def normalizeJson(s: String): String =
    s.replaceAll("\\s+", "")

  def prettyJson(value: Any): String = 
    pretty(render(toJson(value.asInstanceOf[Expr | Statement])))

  test("toJson handles simple constant") {
    val expr = Constant(5)
    val expected = """{
      |  "type": "Constant",
      |  "value": 5
      |}""".stripMargin
    assert(normalizeJson(prettyJson(expr)) == normalizeJson(expected))
  }

  test("toJson handles variable") {
    val expr = Variable("x")
    val expected = """{
      |  "type": "Variable",
      |  "name": "x"
      |}""".stripMargin
    assert(normalizeJson(prettyJson(expr)) == normalizeJson(expected))
  }

  test("toJson handles binary operation") {
    val expr = Plus(Constant(3), Times(Variable("x"), Constant(2)))
    val expected = """{
      |  "type": "Plus",
      |  "left": {
      |    "type": "Constant",
      |    "value": 3
      |  },
      |  "right": {
      |    "type": "Times",
      |    "left": {
      |      "type": "Variable",
      |      "name": "x"
      |    },
      |    "right": {
      |      "type": "Constant",
      |      "value": 2
      |    }
      |  }
      |}""".stripMargin
    assert(normalizeJson(prettyJson(expr)) == normalizeJson(expected))
  }

  test("toJson handles unary minus") {
    val expr = UMinus(Constant(5))
    val expected = """{
      |  "type": "UMinus",
      |  "expr": {
      |    "type": "Constant",
      |    "value": 5
      |  }
      |}""".stripMargin
    assert(normalizeJson(prettyJson(expr)) == normalizeJson(expected))
  }

  test("toJson handles assignment") {
    val stmt = Assignment("x", Plus(Variable("x"), Constant(1)))
    val expected = """{
      |  "type": "Assignment",
      |  "variable": "x",
      |  "expr": {
      |    "type": "Plus",
      |    "left": {
      |      "type": "Variable",
      |      "name": "x"
      |    },
      |    "right": {
      |      "type": "Constant",
      |      "value": 1
      |    }
      |  }
      |}""".stripMargin
    assert(normalizeJson(prettyJson(stmt)) == normalizeJson(expected))
  }

  test("toJson handles if statement") {
    val stmt = If(
      Variable("x"),
      Block(List(Assignment("y", Constant(1)))),
      None
    )
    val expected = """{
      |  "type": "If",
      |  "condition": {
      |    "type": "Variable",
      |    "name": "x"
      |  },
      |  "thenBlock": {
      |    "type": "Block",
      |    "statements": [{
      |      "type": "Assignment",
      |      "variable": "y",
      |      "expr": {
      |        "type": "Constant",
      |        "value": 1
      |      }
      |    }]
      |  },
      |  "elseBlock": null
      |}""".stripMargin
    assert(normalizeJson(prettyJson(stmt)) == normalizeJson(expected))
  }

  test("toJson handles while loop") {
    val stmt = While(
      Variable("x"),
      Block(List(
        Assignment("x", Minus(Variable("x"), Constant(1)))
      ))
    )
    val expected = """{
      |  "type": "While",
      |  "condition": {
      |    "type": "Variable",
      |    "name": "x"
      |  },
      |  "body": {
      |    "type": "Block",
      |    "statements": [{
      |      "type": "Assignment",
      |      "variable": "x",
      |      "expr": {
      |        "type": "Minus",
      |        "left": {
      |          "type": "Variable",
      |          "name": "x"
      |        },
      |        "right": {
      |          "type": "Constant",
      |          "value": 1
      |        }
      |      }
      |    }]
      |  }
      |}""".stripMargin
    assert(normalizeJson(prettyJson(stmt)) == normalizeJson(expected))
  }

  test("toJson handles complex nested structure") {
    val stmt = Block(List(
      Assignment("x", Constant(5)),
      If(
        Plus(Variable("x"), Constant(0)),
        Block(List(
          While(
            Variable("x"),
            Block(List(
              Assignment("x", Minus(Variable("x"), Constant(1)))
            ))
          )
        )),
        Some(Block(List(
          Assignment("x", Constant(0))
        )))
      )
    ))
    val expected = """{
      |  "type": "Block",
      |  "statements": [{
      |    "type": "Assignment",
      |    "variable": "x",
      |    "expr": {
      |      "type": "Constant",
      |      "value": 5
      |    }
      |  }, {
      |    "type": "If",
      |    "condition": {
      |      "type": "Plus",
      |      "left": {
      |        "type": "Variable",
      |        "name": "x"
      |      },
      |      "right": {
      |        "type": "Constant",
      |        "value": 0
      |      }
      |    },
      |    "thenBlock": {
      |      "type": "Block",
      |      "statements": [{
      |        "type": "While",
      |        "condition": {
      |          "type": "Variable",
      |          "name": "x"
      |        },
      |        "body": {
      |          "type": "Block",
      |          "statements": [{
      |            "type": "Assignment",
      |            "variable": "x",
      |            "expr": {
      |              "type": "Minus",
      |              "left": {
      |                "type": "Variable",
      |                "name": "x"
      |              },
      |              "right": {
      |                "type": "Constant",
      |                "value": 1
      |              }
      |            }
      |          }]
      |        }
      |      }]
      |    },
      |    "elseBlock": {
      |      "type": "Block",
      |      "statements": [{
      |        "type": "Assignment",
      |        "variable": "x",
      |        "expr": {
      |          "type": "Constant",
      |          "value": 0
      |        }
      |      }]
      |    }
      |  }]
      |}""".stripMargin
    assert(normalizeJson(prettyJson(stmt)) == normalizeJson(expected))
  }

end TestToJson