package ftanml.objects

import java.io.StringWriter
import java.io.Writer
import ftanml.types.FtanType

abstract class FtanValue {
  def writeFtanML(writer: Writer)

  def toFtanML: String = {
    val writer = new StringWriter
    writeFtanML(writer)
    writer.toString
  }

  protected def writeFtanMLName(writer: Writer) {
    writeFtanML(writer)
  }
  
  def writeJson(writer: Writer)
  
  def toJson: String = {
    val writer = new StringWriter
    writeJson(writer)
    writer.toString
  }

  def isInstance (aType : FtanType) : Boolean = {
    aType.matches(this)
  }
}
