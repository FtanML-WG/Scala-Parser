package ftanml.expr

import ftanml.exec.Context
import ftanml.objects.FtanBoolean._
import ftanml.objects.FtanBoolean

/**
 * Binary "and" expression
 */

class AndExpr(lhs:Expression, rhs:Expression) extends Expression(List(lhs, rhs)) {
  override def evaluate(context: Context) = {
    FtanBoolean(
      lhs.evaluate(context).asBoolean("first argument of &&") &&
      rhs.evaluate(context).asBoolean("second argument of &&")
    )
  }

  override def toString = {
    lhs + " && " + rhs
  }
}