package edu.luc.cs.laufer.cs371.expressions

import util.Try
import Expr.*

object behaviors:
  /** Environment for storing variable values. */
  type Environment = scala.collection.mutable.Map[String, Int]

  /** Evaluates an expression with a given environment. */
  def evaluateR(e: Expr)(using env: Environment): Int = e match
    // Value expressions
    case Constant(c) => c
    case Variable(name) => env.getOrElse(name, throw IllegalArgumentException(s"undefined variable: $name"))
    
    // Arithmetic expressions
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r)
    case Minus(l, r) => evaluateR(l) - evaluateR(r)
    case Times(l, r) => evaluateR(l) * evaluateR(r)
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)
    
    // Statement expressions - they evaluate to their last value or 0
    case Block(expressions) => 
      expressions.foldLeft(0)((_, expr) => evaluateR(expr))
    case ExpressionStmt(expr) => 
      evaluateR(expr)
    case Assignment(variable, expr) =>
      val value = evaluateR(expr)
      env(variable) = value
      value
    case If(condition, thenBlock, elseBlock) =>
      if evaluateR(condition) != 0 then
        evaluateR(thenBlock)
      else
        elseBlock.map(evaluateR).getOrElse(0)
    case While(condition, body) =>
      var result = 0
      while evaluateR(condition) != 0 do
        result = evaluateR(body)
      result

  def evaluate(e: Expr): Try[Int] = Try {
    given env: Environment = scala.collection.mutable.Map.empty
    evaluateR(e)
  }

  def size(e: Expr): Int = e match
    // Value expressions
    case Constant(_) | Variable(_) => 1
    
    // Arithmetic expressions
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    
    // Statement expressions
    case Block(expressions) => 1 + expressions.map(size).sum
    case ExpressionStmt(expr) => 1 + size(expr)
    case Assignment(_, expr) => 1 + size(expr)
    case If(cond, thenBlock, elseBlock) =>
      1 + size(cond) + size(thenBlock) + elseBlock.map(size).getOrElse(0)
    case While(cond, body) => 1 + size(cond) + size(body)

  def height(e: Expr): Int = e match
    // Value expressions
    case Constant(_) | Variable(_) => 1
    
    // Arithmetic expressions
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
    
    // Statement expressions
    case Block(expressions) => 
      1 + expressions.map(height).maxOption.getOrElse(0)
    case ExpressionStmt(expr) => 1 + height(expr)
    case Assignment(_, expr) => 1 + height(expr)
    case If(cond, thenBlock, elseBlock) =>
      1 + math.max(height(cond), 
          math.max(height(thenBlock), elseBlock.map(height).getOrElse(0)))
    case While(cond, body) => 1 + math.max(height(cond), height(body))

  /** Converts an AST back to a source code string with proper formatting */
  def unparse(e: Expr, level: Int = 0): String =
    val indent = "  " * level // Two spaces per level
    
    def parenthesize(expr: Expr): String = expr match
      case _: (Constant | Variable) => expr match
        case Constant(c) => c.toString
        case Variable(name) => name
        case _ => throw IllegalArgumentException("impossible case")
      case _ => s"(${unparse(expr, 0)})"

    e match
      // Value expressions
      case Constant(c) => s"$indent$c"
      case Variable(name) => s"$indent$name"
      
      // Arithmetic expressions
      case UMinus(r) => s"$indent-${parenthesize(r)}"
      case Plus(l, r) => s"$indent${parenthesize(l)} + ${parenthesize(r)}"
      case Minus(l, r) => s"$indent${parenthesize(l)} - ${parenthesize(r)}"
      case Times(l, r) => s"$indent${parenthesize(l)} * ${parenthesize(r)}"
      case Div(l, r) => s"$indent${parenthesize(l)} / ${parenthesize(r)}"
      case Mod(l, r) => s"$indent${parenthesize(l)} % ${parenthesize(r)}"
      
      // Statement expressions
      case ExpressionStmt(expr) => 
        s"$indent${unparse(expr, 0)};"
      case Assignment(variable, expr) =>
        s"$indent$variable = ${unparse(expr, 0)};"
      case If(condition, thenBlock, elseBlock) =>
        val elseStr = elseBlock match
          case Some(block) => s" else {\n${unparse(block, level + 1)}\n$indent}"
          case None => s" else {\n$indent}"
        s"""$indent if (${unparse(condition, 0)}) {
           |${unparse(thenBlock, level + 1)}
           |$indent}$elseStr""".stripMargin
      case While(condition, body) =>
        s"""$indent while (${unparse(condition, 0)}) {
           |${unparse(body, level + 1)}
           |$indent}""".stripMargin
      case Block(expressions) =>
        if expressions.isEmpty then s"$indent{}"
        else
          val stmtStrings = expressions.map(unparse(_, level + 1)).mkString("\n")
          s"$indent{\n$stmtStrings\n$indent}"

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  /** Converts an AST to a JSON representation */
  def toJson(e: Expr): JValue = e match
    // Value expressions
    case Constant(c) => 
      ("type" -> "Constant") ~ ("value" -> c)
    case Variable(name) => 
      ("type" -> "Variable") ~ ("name" -> name)
    
    // Arithmetic expressions
    case UMinus(r) => 
      ("type" -> "UMinus") ~ ("expr" -> toJson(r))
    case Plus(l, r) => 
      ("type" -> "Plus") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Minus(l, r) => 
      ("type" -> "Minus") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Times(l, r) => 
      ("type" -> "Times") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Div(l, r) => 
      ("type" -> "Div") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    case Mod(l, r) => 
      ("type" -> "Mod") ~ ("left" -> toJson(l)) ~ ("right" -> toJson(r))
    
    // Statement expressions
    case Block(expressions) => 
      ("type" -> "Block") ~ ("expressions" -> expressions.map(toJson))
    case ExpressionStmt(expr) => 
      ("type" -> "ExpressionStmt") ~ ("expr" -> toJson(expr))
    case Assignment(variable, expr) => 
      ("type" -> "Assignment") ~ ("variable" -> variable) ~ ("expr" -> toJson(expr))
    case If(condition, thenBlock, elseBlock) =>
      ("type" -> "If") ~
      ("condition" -> toJson(condition)) ~
      ("thenBlock" -> toJson(thenBlock)) ~
      ("elseBlock" -> elseBlock.map(toJson))
    case While(condition, body) =>
      ("type" -> "While") ~
      ("condition" -> toJson(condition)) ~
      ("body" -> toJson(body))

end behaviors
