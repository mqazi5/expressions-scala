package edu.luc.cs.laufer.cs371.expressions

import Expr.*

/**
 * Factory for building an AST from the parse results.
 */
object ASTBuilder extends ExprParser:
  // ASTBuilder is no longer needed since we moved the parsing functionality 
  // directly into ExprParser trait with the unified AST
end ASTBuilder
