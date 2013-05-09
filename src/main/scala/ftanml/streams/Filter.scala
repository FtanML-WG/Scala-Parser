package ftanml.streams

/**
 * A Filter is an Acceptor that passes events to an other Acceptor, typically
 * making changes along the way. The base class, designed to be inherited from,
 * makes no changes to the stream
 */

class Filter(out : Acceptor) extends Acceptor {
  def processString(value: String) {
    out.processString(value)
  }

  def processNumber(value: Double) {
    out.processNumber(value)
  }

  def processBoolean(value: Boolean) {
    out.processBoolean(value)
  }

  def processNull() {
    out.processNull()
  }

  def processStartArray() {
    out.processStartArray()
  }

  def processEndArray() {
    out.processEndArray()
  }

  def processStartText() {
    out.processStartText()
  }

  def processEndText() {
    out.processEndText()
  }

  def processStartElement(name: Option[String]) {
    out.processStartElement(name)
  }

  def processAttributeName(name: String) {
    out.processAttributeName(name)
  }

  def processStartContent(isElementOnly: Boolean) {
    out.processStartContent(isElementOnly)
  }

  def processEndElement() {
    out.processEndElement()
  }

  def error(err: Exception) {
    out.error(err)
  }
}