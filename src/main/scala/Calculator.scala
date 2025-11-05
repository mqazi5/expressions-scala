package edu.luc.cs.laufer.cs371.expressions

object Calculator:

  def processExpr(input: String): Unit =
    println("You entered: " + input)
    val result = ASTBuilder.parseAll(ASTBuilder.expr, input)  // Parse expressions
    if result.isEmpty then
      println("This expression could not be parsed")
    else
      import org.json4s.native.JsonMethods.{pretty, render}
      import behaviors.*
      val expr = result.get
      println("The resulting expression is: " + expr)
      println("The corresponding JSON structure is:")
      println(pretty(render(toJson(expr))))
      println("It has size " + size(expr) + " and height " + height(expr))
      println("It evaluates to " + evaluate(expr))

  def main(args: Array[String]): Unit =
    if args.length > 0 then
      processExpr(args mkString " ")
    else
      print("Enter infix expression: ")
      scala.io.Source.stdin.getLines() foreach:
        line =>
          processExpr(line)
          print("Enter infix expression: ")

end Calculator
