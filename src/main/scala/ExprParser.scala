package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import Expr.*

/**
 * Parser for a simple imperative language with arithmetic expressions.
 * Grammar:
 *   repl       ::= expr*
 *   expr       ::= term { { "+" | "-" } term }*
 *                | exprStmt | assignment | if | while | block
 *   exprStmt   ::= expr ";"
 *   assignment ::= ident "=" expr ";"
 *   if        ::= "if" "(" expr ")" block [ "else" block ]
 *   while     ::= "while" "(" expr ")" block
 *   block     ::= "{" expr* "}"
 *   term      ::= factor { { "*" | "/" | "%" } factor }*
 *   factor    ::= ident | number | "+" factor | "-" factor | "(" expr ")"
 *   ident     ::= [a-zA-Z][a-zA-Z0-9_]*
 */
trait ExprParser extends JavaTokenParsers:
  /**
   * Enable missing typesafe equality for `~`.
   * TODO remove once the combinator parser library provides this.
   */
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived

  /** repl ::= expr* */
  def repl: Parser[Expr] = rep(expr) ^^ (exprs => Block(exprs))

  /** expr ::= term { { "+" | "-" } term }* | exprStmt | assignment | if | while | block */
  def expr: Parser[Expr] = 
    arithmetic
    | exprStmt
    | assignment
    | ifStatement
    | whileLoop
    | block

  /** arithmetic ::= term { { "+" | "-" } term }* */
  def arithmetic: Parser[Expr] = term ~ opt(("+" | "-") ~ term) ^^ {
    case base ~ None => base
    case base ~ Some(op ~ next) => 
      if op == "+" then Plus(base, next)
      else Minus(base, next)
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] = factor ~ opt(("*" | "/" | "%") ~ factor) ^^ {
    case factor ~ None => factor
    case factor ~ Some(op ~ next) =>
      if op == "*" then Times(factor, next)
      else if op == "/" then Div(factor, next)
      else Mod(factor, next)
  }

  /** factor ::= ident | number | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = 
    ident ^^ Variable
    | wholeNumber ^^ (n => Constant(n.toInt))
    | "+" ~> factor
    | "-" ~> factor ^^ UMinus
    | "(" ~> expr <~ ")"

  /** exprStmt ::= expr ";" */
  def exprStmt: Parser[Expr] = 
    expr <~ ";" ^^ ExpressionStmt

  /** assignment ::= ident "=" expr ";" */
  def assignment: Parser[Expr] =
    ident ~ ("=" ~> expr <~ ";") ^^ {
      case variable ~ expr => Assignment(variable, expr)
    }

  /** block ::= "{" expr* "}" */
  def block: Parser[Block] =
    "{" ~> rep(expr) <~ "}" ^^ Block.apply

  /** if ::= "if" "(" expr ")" block [ "else" block ] */
  def ifStatement: Parser[Expr] =
    ("if" ~> "(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ {
      case cond ~ thenBlock ~ elseBlock => If(cond, thenBlock, elseBlock)
    }

  /** while ::= "while" "(" expr ")" block */
  def whileLoop: Parser[Expr] =
    ("while" ~> "(" ~> expr <~ ")") ~ block ^^ {
      case cond ~ body => While(cond, body)
    }

  /** ident ::= [a-zA-Z][a-zA-Z0-9_]* */
  override def ident: Parser[String] = 
    """[a-zA-Z][a-zA-Z0-9_]*""".r
  
end ExprParser
