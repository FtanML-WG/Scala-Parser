package ftanml.expr

import ftanml.exec.Context
import ftanml.objects.{FtanNull, FtanFunction, FtanValue}

/**
 * Expression representing a function call
 */

class FunctionCall(operands: Seq[Expression]) extends Expression(operands) {

  def this(theFunction: FtanFunction, arguments: Seq[Expression]) =
    this(new Literal(theFunction.asInstanceOf[FtanValue]) :: arguments.toList)

  override def evaluate(context: Context): FtanValue = {
    operands(0).evaluate(context) match {
      case f: FtanFunction => f(context, operands.tail.map{_.evaluate(context)})
      case FtanNull => FtanNull
      case _ => throw new ClassCastException("First argument of call() must be a function")
    }
  }
}