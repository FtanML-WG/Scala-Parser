package ftanml.objects

import java.io.StringWriter
import java.io.Writer
import ftanml.types.FtanType
import ftanml.streams.Acceptor

abstract class FtanValue {

  /**
   * Send a stream of events representing the content of this value
   * to a specified Acceptor.
   */
  def send(acceptor : Acceptor)
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
