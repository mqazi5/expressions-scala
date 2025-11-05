package edu.luc.cs.laufer.cs371.expressions

import util.Try
import Expr.*
import Statement.*

object behaviors:

  private def evaluateR(e: Expr): Int = e match
    case Constant(c) => c
    case UMinus(r)   => -evaluateR(r)
    case Plus(l, r)  => evaluateR(l) + evaluateR(r)
    case Minus(l, r) => evaluateR(l) - evaluateR(r)
    case Times(l, r) => evaluateR(l) * evaluateR(r)
    case Div(l, r)   => evaluateR(l) / evaluateR(r)
    case Mod(l, r)   => evaluateR(l) % evaluateR(r)
    case Variable(_) => throw IllegalArgumentException("cannot evaluate variable")

  def evaluate(e: Expr): Try[Int] = Try(evaluateR(e))

  def size(e: Expr): Int = e match
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    case Variable(_) => 1

  def height(e: Expr): Int = e match
    case Constant(c) => 1 
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
    case Variable(_) => 1

  /** Converts an AST back to a source code string with proper formatting */
  def unparse(tree: Expr | Statement, level: Int = 0): String =
    val indent = "  " * level // Two spaces per level
    
    def parenthesize(expr: Expr): String = expr match
      case _: (Constant | Variable) => toExprString(expr)
      case _ => s"(${toExprString(expr)})"

    def toExprString(expr: Expr): String = expr match
      case Constant(c) => c.toString
      case Variable(name) => name
      case UMinus(r) => s"-${parenthesize(r)}"
      case Plus(l, r) => s"${parenthesize(l)} + ${parenthesize(r)}"
      case Minus(l, r) => s"${parenthesize(l)} - ${parenthesize(r)}"
      case Times(l, r) => s"${parenthesize(l)} * ${parenthesize(r)}"
      case Div(l, r) => s"${parenthesize(l)} / ${parenthesize(r)}"
      case Mod(l, r) => s"${parenthesize(l)} % ${parenthesize(r)}"

    tree match
      // Handle expressions
      case expr: Expr => toExprString(expr)
      
      // Handle statements
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
      
      case Block(statements) =>
        if statements.isEmpty then s"$indent{}"
        else
          val stmtStrings = statements.map(unparse(_, level + 1)).mkString("\n")
          s"$indent{\n$stmtStrings\n$indent}"

  import org.json4s.JsonAST.JValue
  import org.json4s.JsonDSL.*

  /** Converts an AST to a JSON representation */
  def toJson(tree: Expr | Statement): JValue = tree match
    // Expression cases
    case Constant(c) => 
      ("type" -> "Constant") ~ ("value" -> c)
    case Variable(name) => 
      ("type" -> "Variable") ~ ("name" -> name)
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
    
    // Statement cases
    case Block(statements) => 
      ("type" -> "Block") ~ ("statements" -> statements.map(toJson))
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
