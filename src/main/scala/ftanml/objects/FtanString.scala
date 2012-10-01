package ftanml.objects

import java.io.Writer
import java.lang.IllegalStateException
import ftanml.streams.Acceptor
import ftanml.util.Unicode

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
        Unicode.surrogatePair(Integer.parseInt(input.substring(2, input.length()-1), 16))
      } else if (input.charAt(1) == '[') {
        // 'CDATA' section \[*....*] where * is any character
        input.substring(3, input.length()-2)
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


}

case class FtanString(value: String) extends FtanValue with SizedObject {

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


  def isValidName = value.matches("[\\p{Alpha}][\\p{Alpha}\\p{Digit}_:]*")

//  override def writeFtanMLName(writer: Writer) {
//    if (isValidName)
//      writer.append(value);
//    else
//      writeFtanML(writer);
//  }

  override def send(acceptor: Acceptor) {
    acceptor.processString(value)
  }

  def writeFtanMLContent(writer: Writer) {
    val escapeContent =
      value
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll(">", "\\\\>")
        .replaceAll("\\<", "\\\\<");

    writer.append(escapeContent);
  }

  def size = value.length
}