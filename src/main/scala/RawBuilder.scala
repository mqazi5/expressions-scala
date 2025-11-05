package edu.luc.cs.laufer.cs371.expressions

import Expr.*
import Statement.*

object RawBuilder extends ExprParser[Expr, Statement]:
  def onExpr: Expr ~ Option[String ~ Expr] => Expr = {
    case base ~ None => base
    case base ~ Some(op ~ next) => 
      if op == "+" then Plus(base, next)
      else Minus(base, next)
  }

  def onTerm: Expr ~ Option[String ~ Expr] => Expr = {
    case factor ~ None => factor
    case factor ~ Some(op ~ next) =>
      if op == "*" then Times(factor, next)
      else if op == "/" then Div(factor, next)
      else Mod(factor, next)
  }

  def onNumber: String => Expr = s => Constant(s.toInt)
  
  def onPlusFactor: Expr => Expr = e => e
  
  def onMinusFactor: Expr => Expr = e => UMinus(e)
  
  def onParenExpr: Expr => Expr = e => e

  def onVariable: String => Expr = name => Variable(name)
  
  def onBlock: List[Statement] => Statement = statements => Block(statements)
  
  def onAssignment: String ~ Expr => Statement = {
    case variable ~ expr => Assignment(variable, expr)
  }
  
  def onIf: Expr ~ Statement ~ Option[Statement] => Statement = {
    case condition ~ thenBlock ~ elseBlock => 
      val thenBlockAsBlock: Block = thenBlock match {
        case b: Block => b
        case other => Block(List(other))
      }
      val elseBlockAsBlock: Option[Block] = elseBlock.map {
        case b: Block => b
        case other => Block(List(other))
      }
      If(condition, thenBlockAsBlock, elseBlockAsBlock)
  }
  
  def onWhile: Expr ~ Statement => Statement = {
    case condition ~ body => 
      val bodyAsBlock: Block = body match {
        case b: Block => b
        case other => Block(List(other))
      }
      While(condition, bodyAsBlock)
  }

  def onExpressionStmt: Expr => Statement = e => ExpressionStmt(e)
end RawBuilder
