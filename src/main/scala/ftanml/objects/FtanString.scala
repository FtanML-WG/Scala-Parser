package ftanml.objects

import java.io.Writer
import java.lang.IllegalStateException
import ftanml.streams.Acceptor
import ftanml.util.Unicode
import scala.util.matching.Regex

object FtanString extends FtanString("") {

  /**
   * Process a string containing escape sequences. Return the string with the escape
   * sequences expanded.
   */
  def unescapeString(input: String): String = {
    val r = """\\[snrtS<>"'{\\]|\\x([a-fA-F0-9]+);|\\\[(.)(.*?)\2\]|\\[ \n\r\t]+""".r
    r.replaceAllIn(input, m =>
      Regex.quoteReplacement(
        m.matched.charAt(1) match {
          case '"' => "\""
          case '\'' => "\'"
          case '\\' => "\\"
          case '|' => "|"
          case '{' => "{"
          case 'n' => "\n"
          case 'r' => "\r"
          case 't' => "\t"
          case 's' => " "
          case 'S' => "\u00A0"
          case '<' => "<"
          case '>' => ">"
          case 'x' => Unicode.surrogatePair(Integer.parseInt(m.group(1), 16))
          case '[' => m.group(3)
          case _ => ""
      }))
  }


}

case class FtanString(value: String) extends FtanValue with SizedObject with TextComponent {

  private def escapedValue(usedQuote: Char): String = {
    def escapeChar(input: Char): String = input match {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case char if char == usedQuote => "\\" + usedQuote
      case other =>
        // TODO Allow the user to set a flag which generates a \\xHHHH
        // sequence for non standard characters
        other.toString
    }

    ("" /: value.map(escapeChar))(_ + _)
  }

  def isValidName = FtanElement.VALID_NAME.pattern.matcher(value).matches

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

  def size = Unicode.length(value)
}
