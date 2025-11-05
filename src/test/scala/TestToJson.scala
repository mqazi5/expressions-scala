package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import behaviors.*
import Statement.*
import Expr.*
import org.json4s.native.JsonMethods.*

class TestToJson extends AnyFunSuite:
  
  // Helper method to pretty print JSON
  def prettyJson(value: Any): String = 
    pretty(render(toJson(value.asInstanceOf[Expr | Statement])))

  test("toJson handles simple constant") {
    val expr = Constant(5)
    val expected = """{
      |  "Constant" : 5
      |}""".stripMargin
    assert(prettyJson(expr) == expected)
  }

  test("toJson handles variable") {
    val expr = Variable("x")
    val expected = """{
      |  "Variable" : "x"
      |}""".stripMargin
    assert(prettyJson(expr) == expected)
  }

  test("toJson handles binary operation") {
    val expr = Plus(Constant(3), Times(Variable("x"), Constant(2)))
    val expected = """{
      |  "Plus" : [ {
      |    "Constant" : 3
      |  }, {
      |    "Times" : [ {
      |      "Variable" : "x"
      |    }, {
      |      "Constant" : 2
      |    } ]
      |  } ]
      |}""".stripMargin
    assert(prettyJson(expr) == expected)
  }

  test("toJson handles unary minus") {
    val expr = UMinus(Constant(5))
    val expected = """{
      |  "UMinus" : {
      |    "Constant" : 5
      |  }
      |}""".stripMargin
    assert(prettyJson(expr) == expected)
  }

  test("toJson handles assignment") {
    val stmt = Assignment("x", Plus(Variable("x"), Constant(1)))
    val expected = """{
      |  "Assignment" : {
      |    "variable" : "x",
      |    "expr" : {
      |      "Plus" : [ {
      |        "Variable" : "x"
      |      }, {
      |        "Constant" : 1
      |      } ]
      |    }
      |  }
      |}""".stripMargin
    assert(prettyJson(stmt) == expected)
  }

  test("toJson handles if statement") {
    val stmt = If(
      Variable("x"),
      Block(List(Assignment("y", Constant(1)))),
      None
    )
    val expected = """{
      |  "If" : {
      |    "condition" : {
      |      "Variable" : "x"
      |    },
      |    "thenBlock" : {
      |      "Block" : [ {
      |        "Assignment" : {
      |          "variable" : "y",
      |          "expr" : {
      |            "Constant" : 1
      |          }
      |        }
      |      } ]
      |    },
      |    "elseBlock" : null
      |  }
      |}""".stripMargin
    assert(prettyJson(stmt) == expected)
  }

  test("toJson handles while loop") {
    val stmt = While(
      Variable("x"),
      Block(List(
        Assignment("x", Minus(Variable("x"), Constant(1)))
      ))
    )
    val expected = """{
      |  "While" : {
      |    "condition" : {
      |      "Variable" : "x"
      |    },
      |    "body" : {
      |      "Block" : [ {
      |        "Assignment" : {
      |          "variable" : "x",
      |          "expr" : {
      |            "Minus" : [ {
      |              "Variable" : "x"
      |            }, {
      |              "Constant" : 1
      |            } ]
      |          }
      |        }
      |      } ]
      |    }
      |  }
      |}""".stripMargin
    assert(prettyJson(stmt) == expected)
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
      |  "Block" : [ {
      |    "Assignment" : {
      |      "variable" : "x",
      |      "expr" : {
      |        "Constant" : 5
      |      }
      |    }
      |  }, {
      |    "If" : {
      |      "condition" : {
      |        "Plus" : [ {
      |          "Variable" : "x"
      |        }, {
      |          "Constant" : 0
      |        } ]
      |      },
      |      "thenBlock" : {
      |        "Block" : [ {
      |          "While" : {
      |            "condition" : {
      |              "Variable" : "x"
      |            },
      |            "body" : {
      |              "Block" : [ {
      |                "Assignment" : {
      |                  "variable" : "x",
      |                  "expr" : {
      |                    "Minus" : [ {
      |                      "Variable" : "x"
      |                    }, {
      |                      "Constant" : 1
      |                    } ]
      |                  }
      |                }
      |              } ]
      |            }
      |          }
      |        } ]
      |      },
      |      "elseBlock" : {
      |        "Block" : [ {
      |          "Assignment" : {
      |            "variable" : "x",
      |            "expr" : {
      |              "Constant" : 0
      |            }
      |          }
      |        } ]
      |      }
      |    }
      |  } ]
      |}""".stripMargin
    assert(prettyJson(stmt) == expected)
  }

end TestToJson