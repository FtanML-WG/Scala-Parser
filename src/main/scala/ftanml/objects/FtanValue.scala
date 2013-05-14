package ftanml.objects

import java.io.StringWriter
import java.io.Writer
import ftanml.types.FtanType
import ftanml.streams.{Serializer, Acceptor, JSONSerializerFactory}

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

  /**
   * Get the value serialized as a JSON string (with no indenting)
   */

  def toJson: String = {
    val writer = new StringWriter
    send(JSONSerializerFactory.make(writer, false))
    writer.toString
  }

  def isInstance (aType : FtanType) : Boolean = {
    aType.matches(this)
  }

  def asBoolean (message: String): Boolean = {
    this match {
      case b: FtanBoolean => b.value
      case _ => throw new ClassCastException("The " + message + " must be a boolean")
    }
  }

}
