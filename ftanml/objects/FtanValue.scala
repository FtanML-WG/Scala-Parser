package ftanml.objects

import java.io.StringWriter
import java.io.Writer

abstract class FtanValue {
  def toFtanML(writer: Writer)

  def toFtanML: String = {
    val writer = new StringWriter
    toFtanML(writer)
    writer.toString
  }

  protected def toFtanMLName(writer: Writer) = toFtanML(writer)
}

case class FtanNull extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append("null")
}

case class FtanBoolean(value: Boolean) extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append(value.toString)
}

case class FtanNumber(value: Double) extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append(value.toString)
}
