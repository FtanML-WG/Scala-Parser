package ftanml.objects

import java.io.Writer
import java.lang.IllegalStateException

object FtanString extends FtanString("") {

  /**
   * Process an escape sequence. Returns an String, since the character may be
   * a non-BMP Unicode character
   */
  def deescapeChar(input: String): String = {
    if (input.length == 0)
      // TODO Correct exception class
      throw new IllegalStateException("Empty char can't be deescaped")
    else if (input.charAt(0) == '\\') {
      // Parse default escape sequences
      if (input.length() == 2)
        (input.charAt(1)) match {
          case '"' => "\""
          case '\'' => "\'"
          case '\\' => "\\"
          case '/' => "/"
          case 'b' => "\b"
          case 'f' => "\f"
          case 'n' => "\n"
          case 'r' => "\r"
          case 't' => "\t"
          case '<' => "<"
          case '>' => ">"
          case _ =>
            // TODO Correct exception class
            throw new IllegalStateException("Unknown escape sequence "
              + input)
        }
      else if (input.length == 6 && input.charAt(1) == 'u')
        // Parse unicode escape sequence : \ followed by uHHHH
        Integer.parseInt(input.substring(2, 6), 16).asInstanceOf[Char].toString
      else if (input.charAt(1) == 'x') {
        // Parse unicode escape sequence : \xH+;
        surrogatePair(Integer.parseInt(input.substring(2, input.length()-1), 16))
      } else
        // TODO Correct exception class
        throw new IllegalStateException("Unknown escape sequence "
          + input)
    } else if (input.length() == 1)
      // This isn't an escape sequence, pass the character through
      ""+input.charAt(0)
    else
      // TODO Correct exception class
      throw new IllegalStateException(
        "Multi-Character token found, but isn't an escape sequence")
  }

  /**
   * Convert a Unicode codepoint to a String, either a single-character string
   * in the case of a BMP codepoint, or a surrogate pair in the case of a non-BMP codepoint
   */

  def surrogatePair(code : Int) : String = {
    if (code <= 65535) {
      code.asInstanceOf[Char].toString
    } else {
      "" + highSurrogate(code) + lowSurrogate(code)
    }
  }

  val NONBMP_MIN: Int = 0x10000
  val NONBMP_MAX: Int = 0x10FFFF
  val SURROGATE1_MIN: Char = 0xD800
  val SURROGATE1_MAX: Char = 0xDBFF
  val SURROGATE2_MIN: Char = 0xDC00
  val SURROGATE2_MAX: Char = 0xDFFF

  /**
     * Return the high surrogate of a non-BMP character
     * @param ch The Unicode codepoint of the non-BMP character to be divided.
     * @return the first character in the surrogate pair
     */
  def highSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) >> 10) + SURROGATE1_MIN).asInstanceOf[Char]
  }

    /**
     * Return the low surrogate of a non-BMP character
     * @param ch The Unicode codepoint of the non-BMP character to be divided.
     * @return the second character in the surrogate pair
     */
  def lowSurrogate(ch: Int): Char = {
    (((ch - NONBMP_MIN) & 0x3FF) + SURROGATE2_MIN).asInstanceOf[Char]
  }
}

case class FtanString(value: String) extends FtanValue with GetSize {

  private def escapedValue(usedQuote: Char): String = {
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

  override def writeFtanML(writer: Writer) {
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
    writer.append(usedQuote + escapedValue(usedQuote) + usedQuote);
  }

  def isValidName = value.matches("[\\p{Alpha}][\\p{Alpha}\\p{Digit}_:]*")

  override def writeFtanMLName(writer: Writer) {
    if (isValidName)
      writer.append(value);
    else
      writeFtanML(writer);
  }

  def writeFtanMLContent(writer: Writer) {
    val escapeContent =
      value
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll(">", "\\\\>")
        .replaceAll("\\<", "\\\\<");

    writer.append(escapeContent);
  }

  override def writeJson(writer: Writer)  {
    writer.append("\"" + escapedValue('"') + "\"");
  }
  
  def getSize = value.length
}