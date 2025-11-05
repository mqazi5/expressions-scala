package edu.luc.cs.laufer.cs371.expressions

/** An algebra of arithmetic expressions. */
enum Expr derives CanEqual:
  case Constant(value: Int)
  case Variable(name: String)
  case UMinus(expr: Expr)
  case Plus(left: Expr, right: Expr)
  case Minus(left: Expr, right: Expr)
  case Times(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)

/** An algebra of imperative statements. */
enum Statement derives CanEqual:
  /** A sequence of statements in a block. */
  case Block(statements: List[Statement])
  /** An expression used as a statement (e.g. function call). */
  case ExpressionStmt(expr: Expr)
  /** Variable assignment statement. */
  case Assignment(variable: String, expr: Expr)
  /** Conditional statement with optional else block. */
  case If(condition: Expr, thenBlock: Statement.Block, elseBlock: Option[Statement.Block])
  /** While loop statement. */
  case While(condition: Expr, body: Statement.Block)
