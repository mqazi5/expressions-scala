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
    val env = mutable.Map[String, Int]()
    
    def evaluateInEnv(expr: Expr): Option[Int] = expr match
      case Constant(c) => Some(c)
      case Variable(name) => env.get(name)
      case UMinus(r) => evaluateInEnv(r).map(-_)
      case Plus(l, r) => 
        for
          lv <- evaluateInEnv(l)
          rv <- evaluateInEnv(r)
        yield lv + rv
      case Minus(l, r) =>
        for
          lv <- evaluateInEnv(l)
          rv <- evaluateInEnv(r)
        yield lv - rv
      case Times(l, r) =>
        for
          lv <- evaluateInEnv(l)
          rv <- evaluateInEnv(r)
        yield lv * rv
      case Div(l, r) =>
        for
          lv <- evaluateInEnv(l)
          rv <- evaluateInEnv(r)
          if rv != 0
        yield lv / rv
      case Mod(l, r) =>
        for
          lv <- evaluateInEnv(l)
          rv <- evaluateInEnv(r)
          if rv != 0
        yield lv % rv
      case Block(exprs) =>
        var result: Option[Int] = None
        for expr <- exprs do
          result = evaluateInEnv(expr)
        result
      case ExpressionStmt(expr) =>
        evaluateInEnv(expr) foreach { result =>
          println(s"Result: $result")
        }
        None
      case Assignment(variable, expr) =>
        evaluateInEnv(expr) foreach { value =>
          env(variable) = value
          println(s"$variable = $value")
        }
        None
      case If(condition, thenBlock, elseBlock) =>
        evaluateInEnv(condition) flatMap { value =>
          if value != 0 then
            evaluateInEnv(thenBlock)
          else
            elseBlock.flatMap(evaluateInEnv)
        }
      case While(condition, body) =>
        var lastResult: Option[Int] = None
        var continue = true
        while continue do
          evaluateInEnv(condition) foreach { value =>
            if value != 0 then
              lastResult = evaluateInEnv(body)
            else
              continue = false
          }
        lastResult
    
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
          ASTBuilder.parseAll(ASTBuilder.expr, input) match
            case ASTBuilder.Success(expr, _) =>
              // Show parsed expression/statement
              println("Parsed:")
              println(expr)
              // Evaluate
              evaluateInEnv(expr)
              // Show current environment
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