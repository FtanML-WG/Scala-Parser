package ftanml.objects

import ftanml.exec.Context

trait FtanFunction {
	def apply(context: Context, args: Seq[FtanValue]) : FtanValue
}