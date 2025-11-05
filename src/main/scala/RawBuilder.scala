package edu.luc.cs.laufer.cs371.expressions

object RawBuilder extends ExprParser[Expr, Statement]:
  override def onExpr = identity[Expr]
  override def onTerm = identity[Expr]
  override def onNumber = identity[Expr]
  override def onPlusFactor = identity[Expr]
  override def onMinusFactor = identity[Expr]
  override def onParenExpr = identity[Expr]
  override def onStatement = identity[Statement]
  override def onBlock = identity[Statement]
  override def onAssignment = identity[Statement]
  override def onIf = identity[Statement]
  override def onWhile = identity[Statement]
end RawBuilder
