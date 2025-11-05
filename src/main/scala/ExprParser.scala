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
  def repl: Parser[Expr] = rep(statement) ^^ (exprs => Block(exprs))

  /** statement ::= exprStmt | assignment | if | while | block */
  def statement: Parser[Expr] = 
    exprStmt | assignment | ifStatement | whileLoop | block

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] = chainl1(term, addOp)

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] = chainl1(factor, mulOp)

  /** factor ::= ident | number | prefixOp factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    ident ^^ Variable
    | wholeNumber ^^ (n => Constant(n.toInt))
    | prefixOp ~ factor ^^ { case op ~ e => op(e) }
    | "(" ~> expr <~ ")"
  )

  /** Operator parsers */
  def addOp: Parser[(Expr, Expr) => Expr] = (
    "+" ^^^ { (a: Expr, b: Expr) => Plus(a, b) }
    | "-" ^^^ { (a: Expr, b: Expr) => Minus(a, b) }
  )

  def mulOp: Parser[(Expr, Expr) => Expr] = (
    "*" ^^^ { (a: Expr, b: Expr) => Times(a, b) }
    | "/" ^^^ { (a: Expr, b: Expr) => Div(a, b) }
    | "%" ^^^ { (a: Expr, b: Expr) => Mod(a, b) }
  )

  def prefixOp: Parser[Expr => Expr] = (
    "+" ^^^ { e: Expr => e }
    | "-" ^^^ { e: Expr => UMinus(e) }
  )

  /** Helper for left-associative binary operators */
  def chainl1[T](p: Parser[T], op: Parser[(T, T) => T]): Parser[T] =
    p ~ rep(op ~ p) ^^ {
      case first ~ rest => rest.foldLeft(first) {
        case (acc, op ~ next) => op(acc, next)
      }
    }

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
