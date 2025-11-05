package edu.luc.cs.laufer.cs371.expressions

import Expr.*
import Statement.*

/**
 * Factory for building an AST from the parse results.
 */
object ASTBuilder extends ExprParser[Expr, Statement]:

  // Expression-related parsers
  override def onExpr: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("+" ~ r) => Plus(l, r)
    case l ~ Some("-" ~ r) => Minus(l, r)

  override def onTerm: Expr ~ Option[String ~ Expr] => Expr =
    case l ~ None => l
    case l ~ Some("*" ~ r) => Times(l, r)
    case l ~ Some("/" ~ r) => Div(l, r)
    case l ~ Some("%" ~ r) => Mod(l, r)

  override def onNumber: String => Expr = 
    Constant.apply compose (_.toInt)

  override def onPlusFactor: Expr => Expr = 
    identity

  override def onMinusFactor: Expr => Expr = 
    UMinus.apply

  override def onParenExpr: Expr => Expr = 
    identity

  override def onVariable: String => Expr = 
    Variable.apply

  // Statement-related parsers
  override def onExpressionStmt: Expr => Statement = 
    ExpressionStmt.apply

  override def onAssignment: String ~ Expr => Statement = 
    case variable ~ expr => Assignment(variable, expr)

  override def onIf: Expr ~ Statement ~ Option[Statement] => Statement =
    case condition ~ thenBlock ~ elseBlock => 
      If(condition, thenBlock.asInstanceOf[Block], elseBlock.map(_.asInstanceOf[Block]))

  override def onWhile: Expr ~ Statement => Statement =
    case condition ~ body => While(condition, body.asInstanceOf[Block])

  override def onBlock: List[Statement] => Statement =
    Block.apply

end ASTBuilder
