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

  override def processStartElement(name: Option[String]) {
    if (!stack.isEmpty && (stack.top == middleOfText)) {
      writer.append(',')
    }
    super.processStartElement(name)
  }

  override def processEndElement() {
    super.processEndElement()
    if (!stack.isEmpty && (stack.top == startOfText)) {
      replaceTop(middleOfText)
    }
  }

  override protected def writeElementName(name : String) {
    writer.append("\"$name\":")
    formatString(name)
  }

  override protected def writeAttributeName(name: String) {
    if (!name.isEmpty) {
      formatString(name)
      writer.append(attPunctuation);
    } else {
      formatString("$content")
      writer.append(attPunctuation);
    }
  }

  override protected def writeName(name : String) {
    formatString(name)
  }

  override protected def formatString(value: String) {
    //Output the escaped string
    writer.append('"' + escapedValue(value, '"') + '"');
  }

  override def processStartText() {
    preValue()
    writer.append("[")
    stack.push(startOfText)
  }

  override def processEndText() {
    stack.pop()
    writer.append("]")
    postValue()
  }

  override def processString(value: String) {
    if (!stack.isEmpty && (stack.top == startOfText)) {
      writer.append('"' + escapedValue(value, '<') + '"')
      replaceTop(middleOfText)
    } else if (!stack.isEmpty && (stack.top == middleOfText)) {
      writer.append(",\"" + escapedValue(value, '<') + '"')
    } else {
      preValue()
      formatString(value)
      postValue()
    }
  }

}

object JSONSerializerFactory {
  def make(writer : Writer,  indenting : Boolean) : Acceptor =
   new JSONSerializer(writer, indenting)
}