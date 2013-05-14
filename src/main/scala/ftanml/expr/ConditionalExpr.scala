package ftanml.expr

import ftanml.exec.Context

/**
 * Defines a conditional (if-then-else) expression
 */

class ConditionalExpr(_if: Expression, _then: Expression, _else: Expression) extends Expression(List(_if, _then, _else)) {
  override def evaluate(context: Context) = {
    if (_if.evaluate(context).asBoolean("conditional")) {
      _then.evaluate(context)
    } else {
      _else.evaluate(context)
    }
  }

  override def toString = {
    "if " + _if.toString + " then " + _then.toString + " else " + _else.toString()
  }
}