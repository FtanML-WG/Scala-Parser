package ftanml.streams

import java.io.Writer
import ftanml.util.Unicode
import ftanml.objects.{FtanString, FtanElement}

/**
 * A serializer accepts a stream of events representing a FtanML value and outputs
 * a JSON representation of the value to a Writer. The JSONSerializer itself must be
 * preceded in the pipeline by a ContentExpander that convers the name and content into
 * standard attributes
 */

private class JSONSerializer(writer : Writer, indenting : Boolean) extends Serializer(writer, indenting) {

  protected override val startElement = '{'
  protected override val endElement = '}'
  protected override val attSeparator = ','
  protected override val attPunctuation = ':'

  override protected def writeName(name : String) {
    formatString(name)
  }

  override protected def formatString(value: String) {
    //Output the escaped string
    writer.append('"' + escapedValue(value, '"') + '"');
  }

}

object JSONSerializerFactory {
  def make(writer : Writer,  indenting : Boolean) : Acceptor =
   new ContentExpander(new JSONSerializer(writer, indenting))
}