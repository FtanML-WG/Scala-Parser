package ftanml.expr

import ftanml.exec.Context

/**
 * Expression representing a reference to a function argument
 */

class ParamRef(id: Int) extends Expression(List()) {
  override def evaluate(context: Context) = {
    context.evaluateParam(id)
  }

  override def toString = "_" + id
}