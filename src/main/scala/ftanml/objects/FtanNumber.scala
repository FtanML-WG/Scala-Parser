package ftanml.objects

import java.io.Writer

case class FtanNumber(value: Double) extends FtanValue {
  override def writeFtanML(writer: Writer) {
    writer.append(value.toString)
  }
  override def writeJson(writer: Writer) {
    writer.append(value.toString)
  }
}

object FtanNumber extends FtanNumber(0)