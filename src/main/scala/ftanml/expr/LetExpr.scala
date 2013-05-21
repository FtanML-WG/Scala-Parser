package ftanml.expr

import ftanml.exec.Context

/**
 * A let expression binds a variable and then evaluates a _return expression
 */

class LetExpr(name: String, _select: Expression, _return: Expression) extends Expression(List(_select, _return)) {
  override def evaluate(context: Context) = {
    var value = _select.evaluate(context)
    context.setVar(name, value)
    _return.evaluate(context)
  }

  override def toString = "let " + name + "=(" + _select.toString + "); " + _return.toString
}