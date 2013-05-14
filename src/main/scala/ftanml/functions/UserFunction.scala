package ftanml.functions

import ftanml.expr.Expression
import ftanml.exec.Context
import ftanml.objects.{FtanValue, FtanFunction}
import ftanml.streams.Acceptor

/**
 * A user defined function (evaluated by evaluating the expression that forms
 * its body)
 */
class UserFunction(body: Expression) extends FtanValue with FtanFunction {
  def apply(context: Context, args: Seq[FtanValue]) = {
    body.evaluate(context.newContext(args))
  }

  def send(acceptor: Acceptor) {}
}