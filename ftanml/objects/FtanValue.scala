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
  
  def toJson(writer: Writer)
  
  def toJson: String = {
    val writer = new StringWriter
    toJson(writer)
    writer.toString
  }
}

case class FtanNull extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append("null")
  override def toJson(writer: Writer) = writer.append("null")
}

case class FtanBoolean(value: Boolean) extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append(value.toString)
  override def toJson(writer: Writer) = writer.append(value.toString)
}

case class FtanNumber(value: Double) extends FtanValue {
  override def toFtanML(writer: Writer) = writer.append(value.toString)
  override def toJson(writer: Writer) = writer.append(value.toString)
}
