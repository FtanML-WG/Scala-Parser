package ftanml.objects

import java.io.StringWriter
import java.io.Writer
import ftanml.types.FtanType
import ftanml.streams.{Serializer, Acceptor}

abstract class FtanValue {

  /**
   * Send a stream of events representing the content of this value
   * to a specified Acceptor.
   */
  def send(acceptor : Acceptor)

  /**
   * Get the value serialized as a FtanML string (with no indenting)
   */

  def toFtanML: String = {
    val writer = new StringWriter
    send(new Serializer(writer, false))
    writer.toString
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
