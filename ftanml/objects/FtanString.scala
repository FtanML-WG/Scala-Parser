package ftanml.objects

import java.io.Writer

object FtanString extends FtanString("") {
  def deescapeChar(input: String): Char = {
    if (input.length == 0)
      // TODO Correct exception class
      throw new IllegalStateException("Empty char can't be deescaped")
    else if (input.charAt(0) == '\\') {
      // Parse default escape sequences
      if (input.length() == 2)
        (input.charAt(1)) match {
          case '"' => '"'
          case '\'' => '\''
          case '\\' => '\\'
          case '/' => '/'
          case 'b' => '\b'
          case 'f' => '\f'
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case '<' => '<'
          case '>' => '>'
          case _ =>
            // TODO Correct exception class
            throw new IllegalStateException("Unknown escape sequence "
              + input)
        }
      else if (input.length == 6 && input.charAt(1) == 'u')
        // Parse unicode escape sequence
        Integer.parseInt(input.substring(2, 6), 16).asInstanceOf[Char]
      else
        // TODO Correct exception class
        throw new IllegalStateException("Unknown escape sequence "
          + input)
    } else if (input.length() == 1)
      // This isn't an escape sequence, pass the character through
      input.charAt(0)
    else
      // TODO Correct exception class
      throw new IllegalStateException(
        "Multi-Character token found, but isn't an escape sequence")
  }

}

case class FtanString(value: String) extends FtanValue {

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
}