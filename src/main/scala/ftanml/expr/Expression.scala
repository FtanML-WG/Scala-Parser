package ftanml.expr

import ftanml.exec.Context
import ftanml.objects.{FtanNull, FtanValue}


abstract class Expression(operands: Seq[Expression]) {
  def evaluate(context: Context): FtanValue = FtanNull
}