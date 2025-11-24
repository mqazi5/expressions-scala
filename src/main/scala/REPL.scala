package edu.luc.cs.laufer.cs371.expressions

import scala.io.StdIn.readLine
import behaviors.*
import org.json4s.native.JsonMethods.*
import scala.collection.mutable
import Expr.*

object REPL:

  def main(args: Array[String]): Unit =
    println("Welcome to MiniJS Calculator REPL!")
    println("Enter expressions or statements (empty line to exit)")
    println("Examples:")
    println("  Expression: ((1 + 2) * (3 + 4)) / 2 - 8")
    println("  Statement: x = 5; while (x > 0) { x = x - 1; }")
    
    // Environment to store variables
    // UPDATED: use Value instead of Int
    val env: Environment = mutable.Map.empty

    //evaulate wiht new environmentr 
    def evaluateInEnv(expr: Expr) =
      evaluateWithEnv(expr, env)

    var continue = true
    while continue do
      print("\nminijs> ")
      val input = readLine()
      
      if input.trim.isEmpty then
        continue = false
      else
        try
          println(s"You entered: $input")
          
          // Parse and evaluate input
          val parseResult = ASTBuilder.parseAll(ASTBuilder.statement, input) match
            case success @ ASTBuilder.Success(_, _) => success
            case ASTBuilder.NoSuccess(_, _) =>
              // Try as multiple statements (repl parser)
              ASTBuilder.parseAll(ASTBuilder.repl, input)
          
          parseResult match
            case ASTBuilder.Success(expr, _) =>
              // Show parsed expression/statement
              println("Parsed:")
              println(expr)

              // Evaluate using the new evaluator
              val result = evaluateInEnv(expr)

              // Print the result
              result match
                case scala.util.Success(value) => println(s"Result: $value")
                case scala.util.Failure(ex) => println(s"Error: ${ex.getMessage}")

              // Show current environment (variables)
              if env.nonEmpty then
                println("\nCurrent variable values:")
                env.toList.sortBy(_._1).foreach { (name, value) =>
                  println(s"$name = $value")
                }
            
            case ASTBuilder.NoSuccess(msg, next) =>
              throw Exception(s"Parse error near '${next.pos.longString}': $msg")
          
        catch
          case e: Exception =>
            println(s"Error: ${e.getMessage}")
            println("Please try again")
end REPL
