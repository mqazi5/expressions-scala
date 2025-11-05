package edu.luc.cs.laufer.cs371.expressions

import Expr.*
import Statement.*

object RawBuilder extends ExprParser[Expr, Statement]:
  override def onExpr(e: Expr ~ Option[String ~ Expr]): Expr = e match
    case base ~ None => base
    case base ~ Some(op ~ next) => 
      if op == "+" then Plus(base, next)
      else Minus(base, next)

  override def onTerm(t: Expr ~ Option[String ~ Expr]): Expr = t match
    case factor ~ None => factor
    case factor ~ Some(op ~ next) =>
      if op == "*" then Times(factor, next)
      else if op == "/" then Div(factor, next)
      else Mod(factor, next)

  override def onNumber(s: String): Expr = Constant(s.toInt)
  
  override def onPlusFactor(e: Expr): Expr = e
  
  override def onMinusFactor(e: Expr): Expr = UMinus(e)
  
  override def onParenExpr(e: Expr): Expr = e

  override def onStatement(s: Statement): Statement = s
  
  override def onBlock(statements: List[Statement]): Statement = Block(statements)
  
  override def onAssignment(v: String ~ Expr): Statement = v match
    case variable ~ expr => Assignment(variable, expr)
  
  override def onIf(cond: Expr ~ Statement ~ Option[Statement]): Statement = cond match
    case condition ~ thenBlock ~ elseBlock => 
      val thenBlockAsBlock = thenBlock match
        case b @ Statement.Block(_) => b
        case other => Statement.Block(List(other))
      val elseBlockAsBlock = elseBlock.map {
        case b @ Statement.Block(_) => b
        case other => Statement.Block(List(other))
      }
      Statement.If(condition, thenBlockAsBlock.asInstanceOf[Statement.Block], elseBlockAsBlock.asInstanceOf[Option[Statement.Block]])
  
  override def onWhile(w: Expr ~ Statement): Statement = w match
    case condition ~ body => 
      val bodyAsBlock = body match
        case b @ Statement.Block(_) => b
        case other => Statement.Block(List(other))
      Statement.While(condition, bodyAsBlock.asInstanceOf[Statement.Block])

  override def onExpressionStmt(e: Expr): Statement = ExpressionStmt(e)
end RawBuilder
