package ftanml.expr

import ftanml.objects.FtanValue
import ftanml.exec.Context

/**
 * An expression that encapsulates a fixed literal value
 */

case class Literal(value: FtanValue) extends Expression(List()) {
  override def evaluate(context: Context) = value
}