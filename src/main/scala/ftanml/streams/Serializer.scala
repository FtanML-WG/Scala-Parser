package ftanml.streams

import java.io.Writer
import collection.mutable.Stack
import ftanml.util.Unicode

/**
 * A serializer accepts a stream of events representing a FtanML value and outputs
 * the FtanML representation of the value to a Writer
 */

class Serializer(writer: Writer, indenting: Boolean) extends Acceptor {

  // TODO: rewrite this using enumerations
  protected val topLevel = 0
  protected val startOfArray = 1
  protected val middleOfArray = 2
  protected val startOfElement = 3
  protected val middleOfElement = 4
  protected val inContent = 5

  protected val startElement = '<'
  protected val endElement = '>'
  protected val attSeparator = ' '
  protected val attPunctuation = '='

  protected val stack = new Stack[Int]

  protected def replaceTop(state: Int) {
    stack.pop()
    stack.push(state)
  }

  def processString(value: String) {
    if (!stack.isEmpty && (stack.top == inContent)) {
      writer.append(escapedValue(value, '<'))
    } else {
      preValue()
      formatString(value)
      postValue()
    }
  }

  protected def formatString(value: String) {
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

  /**
   * Backslash-escape any characters in a string that need to be escaped.
   * @param usedQuote the string delimiter (single or double quotes) if the
   * string is to be output in quotes, or the character "<" if the string is
   * to be output as part of mixed content (in which case "<" and ">" need
   * to be escaped)
   */

  protected def escapedValue(value: String, usedQuote: Char): String = {
    def escapeChar(input: Int): String = input match {
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '<' => if ('<' == usedQuote) "\\<" else "<"
      case '>' => if ('<' == usedQuote) "\\>" else ">"
      case char if char == usedQuote => "\\" + usedQuote
      case other =>
        if (other >= Unicode.NONBMP_MIN) {
          "\\x" + other.toHexString + ";"
        } else {
          other.asInstanceOf[Char].toString
        }
    }
    ("" /: Unicode.codepoints(value).map(escapeChar))(_ ++ _)
  }

  def processNumber(value: Double) {
    preValue()
    if (value == value.floor && !value.isInfinite) {
      // use integer notation for whole numbers
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
    writer.append(startElement)
    stack.push(startOfElement)
    name.map { name =>
      writeName(name);
      replaceTop(middleOfElement)
    }
  }

  def processAttributeName(name: String) {
    if (stack.pop() != startOfElement) {
      writer.append(attSeparator);
    }
    stack.push(middleOfElement)
    writeName(name)
    writer.append(attPunctuation);
  }

  def processStartContent(isElementOnly: Boolean) {
    // TODO: omit "|" for element-only content. But note that tests in ElementTest will have to change
    //if (!isElementOnly) {
      writer.append("|")
    //}
    replaceTop(inContent)
  }

  def processEndElement() {
    stack.pop()
    writer.append(endElement)
  }

  def error(err: Exception) {
    writer.close()
  }

  protected def preValue() {
    if (!stack.isEmpty) {
      val state = stack.top
      if (state == startOfArray) {
        replaceTop(middleOfArray)
      } else if (state == middleOfArray) {
        writer.append(",")
      }
    }
  }

  protected def postValue() {

  }

  private def isValidName(name: String) = name.matches("[\\p{Alpha}][\\p{Alpha}\\p{Digit}_:]*")

  protected def writeName(name: String) {
    if (isValidName(name))
      writer.append(name)
    else
      formatString(name)
  }
}