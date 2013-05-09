package ftanml.streams

/**
 * This trait contains a set of methods representing event-based messages
 * communicating the content of a FtanML value from a sender to a recipient.
 * It is a push-based model in which the sender calls methods implemented
 * by the recipient to notify particular kinds of event. There are strict
 * rules about the allowed sequence of calls.
 *
 * A value is sent as one of the following (described recursively):
 *
 * (a) A simple value: string, number, boolean, or null
 *
 * (b) An array: a startArray event, followed by a sequence of values, followed by an endArray event
 *
 * (c) An element: a startElement event, followed by a sequence of pairs of calls each supplying a
 * name followed by a value, followed optionally by (a startContent event followed by a sequence of values
 * representing the content), followed by an endElement event.
 */

trait Acceptor {

  /**
   * Process a string value, including a string value that appears in element content
   */

  def processString(value : String)

  /**
   * Process a numeric value
   */

  def processNumber(value : Double)

  /**
   * Process a boolean value
   */

  def processBoolean(value : Boolean)

  /**
   * Process the null value
   */

  def processNull()

  /**
   * Process a startArray event
   */
  
  def processStartArray()

  /**
   * Process an endArray event
   */

  def processEndArray()

  /**
   * Process a startElement event
   */

  def processStartElement(name : Option[String])

  /**
   * Process an attribute name. The attribute value will follow in the next call, or in the next
   * start/end bracketed sequence of calls
   */

  def processAttributeName(name : String)

  /**
   * Process the start of the content of an element. The isElementOnly flag
   * should be set to true if it is known that the content is element-only; it can
   * be set to false if it is not known what the content will be
   */

  def processStartContent(isElementOnly : Boolean)

  /**
   * Process an endElement event
   */

  def processEndElement()

  /**
   * Process a startText event
   */

  def processStartText()

  /**
   * Process an endText event
   */

  def processEndText()

  /**
   * Notify the acceptor that an error has occurred and no more events will be sent
   */

  def error(err : Exception)

}