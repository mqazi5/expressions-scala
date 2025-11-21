package edu.luc.cs.laufer.cs371.expressions

import util.Try
import scala.util.{Success, Failure}
import Expr.*
import edu.luc.cs.laufer.cs371.expressions.*

object behaviors:
  /** Environment for storing variable values. */ 
  //needs to be mutable
  type Environment = scala.collection.mutable.Map[String, Value]

  def evaluateR(e: Expr)(using env: Environment): Try[Value] = e match

    case Constant(c) =>
      Success(Num(c))

    case Variable(name) =>
      env.get(name) match
        case Some(v) => Success(v)
        case None    => Failure(new NoSuchFieldException(name))

    case UMinus(r) =>
      evaluateR(r).map { case Num(v) => Num(-v) }

    case Plus(l, r) =>
      for
        case Num(a) <- evaluateR(l)
        case Num(b) <- evaluateR(r)
      yield Num(a + b)

    case Minus(l, r) =>
      for
        case Num(a) <- evaluateR(l)
        case Num(b) <- evaluateR(r)
      yield Num(a - b)

    case Times(l, r) =>
      for
        case Num(a) <- evaluateR(l)
        case Num(b) <- evaluateR(r)
      yield Num(a * b)

    case Div(l, r) =>
      for
        case Num(a) <- evaluateR(l)
        case Num(b) <- evaluateR(r)
        res <- if b == 0 then Failure(new ArithmeticException("division by zero"))
               else Success(Num(a / b))
      yield res

    case Mod(l, r) =>
      for
        case Num(a) <- evaluateR(l)
        case Num(b) <- evaluateR(r)
        res <- if b == 0 then Failure(new ArithmeticException("mod by zero"))
               else Success(Num(a % b))
      yield res

    case Assignment(variable, expr) =>
      evaluateR(expr).map { v =>
        env(variable) = v
        Num(0)   // assignment evaluates to void
      }

    case ExpressionStmt(expr) =>
      evaluateR(expr).map(_ => Num(0))

    case Block(expressions) =>
      expressions.foldLeft(Success(Num(0)): Try[Value]) {
        (acc, expr) => acc.flatMap(_ => evaluateR(expr))
      }

    case If(condition, thenBlock, elseBlock) =>
      evaluateR(condition).flatMap {
        case Num(v) =>
          if v != 0 then evaluateR(thenBlock)
          else elseBlock.map(evaluateR).getOrElse(Success(Num(0)))
      }

    case While(condition, body) =>
      def loop(): Try[Value] =
        evaluateR(condition).flatMap {
          case Num(v) if v != 0 =>
            evaluateR(body).flatMap(_ => loop())
          case Num(_) =>
            Success(Num(0))
        }
      loop()

  /** Entry point for evaluating with a fresh environment. */
  def evaluate(e: Expr): Try[Value] =
    val env: Environment = scala.collection.mutable.Map.empty
    evaluateWithEnv(e, env)

  /** Evaluate while exposing the environment (used by REPL). */
  def evaluateWithEnv(e: Expr, env: Environment): Try[Value] =
    given Environment = env
    evaluateR(e)

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
      case Constant(c) => c.toString
      case Variable(name) => name
      case UMinus(r) => s"(${unparse(expr, 0)})"
      case _ => s"(${unparse(expr, 0)})"

    def unparseBlockBody(b: Block, level: Int): String = b match
      case Block(expressions) if expressions.isEmpty => ""
      case Block(expressions) => expressions.map(unparse(_, level)).mkString("\n")

    e match
      // Value expressions
      case Constant(c) => s"$indent$c"
      case Variable(name) => s"$indent$name"
      
      // Arithmetic expressions
      case UMinus(r) => s"$indent(-${unparse(r, 0)})"
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
        val thenBody = unparseBlockBody(thenBlock, level + 1)
        val elseBody = elseBlock.map(b => unparseBlockBody(b, level + 1)).getOrElse("")
        val elseStr = elseBlock match
          case Some(_) => s" else {\n${elseBody}\n$indent}"
          case None => s" else {\n$indent}"
        s"${indent}if (${unparse(condition, 0)}) {\n${thenBody}\n${indent}}${elseStr}"

      case While(condition, body) =>
        val bodyStr = unparseBlockBody(body, level + 1)
        s"${indent}while (${unparse(condition, 0)}) {\n${bodyStr}\n${indent}}"
      case Block(expressions) =>
        if expressions.isEmpty then s"$indent{}"
        else
          val stmtStrings = expressions.map(unparse(_, level + 1)).mkString("\n")
          s"$indent{\n$stmtStrings\n$indent}"

  import org.json4s.JsonAST.{JValue, JNull}
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
      ("type" -> "Block") ~ ("statements" -> expressions.map(toJson))
    case ExpressionStmt(expr) => 
      ("type" -> "ExpressionStmt") ~ ("expr" -> toJson(expr))
    case Assignment(variable, expr) => 
      ("type" -> "Assignment") ~ ("variable" -> variable) ~ ("expr" -> toJson(expr))
    case If(condition, thenBlock, elseBlock) =>
      ("type" -> "If") ~
      ("condition" -> toJson(condition)) ~
      ("thenBlock" -> toJson(thenBlock)) ~
      ("elseBlock" -> elseBlock.map(toJson).getOrElse(JNull))
    case While(condition, body) =>
      ("type" -> "While") ~
      ("condition" -> toJson(condition)) ~
      ("body" -> toJson(body))

end behaviors
