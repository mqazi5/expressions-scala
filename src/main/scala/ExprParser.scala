package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*
import Statement.*

/**
 * Parser for a simple imperative language with arithmetic expressions.
 * Grammar:
 *   repl       ::= statement*
 *   statement  ::= expression ";" | assignment | if | while | block
 *   assignment ::= ident "=" expression ";"
 *   if        ::= "if" "(" expression ")" block [ "else" block ]
 *   while     ::= "while" "(" expression ")" block
 *   block     ::= "{" statement* "}"
 *   expr      ::= term { { "+" | "-" } term }*
 *   term      ::= factor { { "*" | "/" | "%" } factor }*
 *   factor    ::= ident | number | "+" factor | "-" factor | "(" expr ")"
 *   ident     ::= [a-zA-Z][a-zA-Z0-9_]*
 */
trait ExprParser[E, S] extends JavaTokenParsers:

  /**
   * Enable missing typesafe equality for `~`.
   * TODO remove once the combinator parser library provides this.
   */
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  /** repl ::= statement* */
  def repl: Parser[S] = rep(statement) ^^ onBlock

  /** statement ::= expression ";" | assignment | if | while | block */
  def statement: Parser[S] = 
    (expr <~ ";") ^^ onExpressionStmt
    | assignment
    | ifStatement
    | whileLoop
    | block

  /** assignment ::= ident "=" expression ";" */
  def assignment: Parser[S] =
    ident ~ ("=" ~> expr <~ ";") ^^ onAssignment

  /** if ::= "if" "(" expression ")" block [ "else" block ] */
  def ifStatement: Parser[S] =
    ("if" ~> "(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ onIf

  /** while ::= "while" "(" expression ")" block */
  def whileLoop: Parser[S] =
    ("while" ~> "(" ~> expr <~ ")") ~ block ^^ onWhile

  /** block ::= "{" statement* "}" */
  def block: Parser[S] =
    "{" ~> rep(statement) <~ "}" ^^ onBlock

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[E] = term ~! opt(("+" | "-") ~ term) ^^ onExpr

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[E] = factor ~! opt(("*" | "/" | "%") ~ factor) ^^ onTerm

  /** factor ::= ident | wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[E] = 
    ident ^^ onVariable
    | wholeNumber ^^ onNumber
    | "+" ~> factor ^^ onPlusFactor
    | "-" ~> factor ^^ onMinusFactor
    | "(" ~> expr <~ ")" ^^ onParenExpr

  /** ident ::= [a-zA-Z][a-zA-Z0-9_]* */
  override def ident: Parser[String] = 
    """[a-zA-Z][a-zA-Z0-9_]*""".r

  // Expression-related abstract methods
  def onExpr: E ~ Option[String ~ E] => E
  def onTerm: E ~ Option[String ~ E] => E
  def onNumber: String => E
  def onPlusFactor: E => E
  def onMinusFactor: E => E
  def onParenExpr: E => E
  def onVariable: String => E

  // Statement-related abstract methods
  def onExpressionStmt: E => S
  def onAssignment: String ~ E => S
  def onIf: E ~ S ~ Option[S] => S
  def onWhile: E ~ S => S
  def onBlock: List[S] => S
  
end ExprParser
