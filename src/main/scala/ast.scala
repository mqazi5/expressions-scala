package edu.luc.cs.laufer.cs371.expressions

/** A unified algebra of expressions and statements. */
enum Expr derives CanEqual:
  // Value expressions
  /** A constant (numeric literal). */
  case Constant(value: Int)
  /** A variable (identifier). */
  case Variable(name: String)
  
  // Arithmetic expressions
  /** Unary minus operation. */
  case UMinus(expr: Expr)
  /** Addition operation. */
  case Plus(left: Expr, right: Expr)
  /** Subtraction operation. */
  case Minus(left: Expr, right: Expr)
  /** Multiplication operation. */
  case Times(left: Expr, right: Expr)
  /** Division operation. */
  case Div(left: Expr, right: Expr)
  /** Modulo operation. */
  case Mod(left: Expr, right: Expr)
  
  // Statement expressions
  /** A sequence of expressions in a block. */
  case Block(expressions: List[Expr])
  /** Expression used as a statement. */
  case ExpressionStmt(expr: Expr)
  /** Variable assignment. */
  case Assignment(variable: String, expr: Expr)
  /** Conditional expression with optional else block. */
  case If(condition: Expr, thenBlock: Block, elseBlock: Option[Block])
  /** While loop expression. */
  case While(condition: Expr, body: Block)
