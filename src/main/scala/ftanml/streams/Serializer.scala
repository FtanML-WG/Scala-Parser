package ftanml.streams

import java.io.Writer
import collection.mutable.Stack

/**
 * A serializer accepts a stream of events representing a FtanML value and outputs
 * the FtanML representation of the value to a Writer
 */

class Serializer(writer : Writer, indenting : Boolean) extends Acceptor {

  // TODO: rewrite this using enumerations
  val topLevel = 0
  val startOfArray = 1
  val middleOfArray = 2
  val startOfElement = 3
  val middleOfElement = 4
  val inContent = 5

  private val stack = new Stack[Int]

  def processString(value: String) {
    if (!stack.isEmpty && (stack.top==inContent)) {
      writer.append(escapedValue(value, '\\'))
    } else {
      preValue()
      formatString(value)
      postValue()
    }
  }

  private def formatString(value: String) {
    //Calculate count of double quotation marks (") and single quotation marks (') in the string
    var numberDQuotes = 0;
    var numberSQuotes = 0;
    value.foreach {
      case '"' => numberDQuotes += 1
      case '\'' => numberSQuotes += 1
      case _ =>
    }
    //Use the quotation mark that causes less escaping
    val usedQuote = if (numberDQuotes <= numberSQuotes) '"' else '\''
    //Output the escaped string
    writer.append(usedQuote + escapedValue(value, usedQuote) + usedQuote);

  }

  private def escapedValue(value: String, usedQuote: Char): String = {
    def escapeChar(input: Char): String = input match {
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case char if char == usedQuote => "\\" + usedQuote
      case other =>
        // TODO Allow the user to set a flag which generates a \\uXXXX
        // sequence for non standard characters
        other.toString
    }
    ("" /: value.map(escapeChar))(_ + _)
  }

  def processNumber(value: Double) {
    preValue()
    if (value.isWhole()) {
      writer.append(value.asInstanceOf[Long].toString)
    } else {
      writer.append(value.toString)
    }
    postValue()
  }

  def processBoolean(value: Boolean) {
    preValue()
    writer.append(value.toString)
    postValue()
  }

  def processNull() {
    preValue()
    writer.append("null")
    postValue()
  }

  def processStartArray() {
    preValue()
    writer.append("[")
    stack.push(startOfArray)
  }

  def processEndArray() {
    stack.pop()
    writer.append("]")
    postValue()
  }

  def processStartElement(name: Option[String]) {
    preValue()
    writer.append("<")
    var needSpace = false
    name match {
      case Some(n) => {
        writeName(n)
        needSpace = true
      }
      case None =>
    }
    stack.push(if (needSpace) middleOfElement else startOfElement)
  }

  def processAttributeName(name: String) {
    if (stack.pop() != startOfElement) {
      writer.append(" ");
    }
    stack.push(middleOfElement)
    writeName(name)
    writer.append("=");
  }

  def processStartContent(isElementOnly: Boolean) {
    if (!isElementOnly) {
      writer.append("|")
    }
    stack.pop()
    stack.push(inContent)
  }

  def processEndElement() {
    stack.pop()
    writer.append(">")
  }

  def error(err: Exception) {
    writer.close()
  }

  private def preValue()  {
    if (!stack.isEmpty) {
      val state = stack.top
      if (state == startOfArray) {
        stack.pop()
        stack.push(middleOfArray)
      } else if (state == middleOfArray) {
        writer.append(",")
      }
    }
  }

  private def postValue() {

  }

  private def isValidName(name : String) = name.matches("[\\p{Alpha}][\\p{Alpha}\\p{Digit}_:]*")

  private def writeName(name : String) {
    if (isValidName(name))
      writer.append(name);
    else
      formatString(name)
  }
}