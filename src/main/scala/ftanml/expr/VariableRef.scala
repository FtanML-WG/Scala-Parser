package ftanml.expr

import ftanml.exec.Context

/**
 * Expression representing a reference to a variable
 */

class VariableRef(name: String) extends Expression(List()) {
  override def evaluate(context: Context) = {
    context.evaluateVar(name)
  }

  override def toString = name
}