package ftanml

import scala.util.parsing.combinator.Parsers
import ftanml.objects.FtanNull
import ftanml.objects.FtanElement
import scala.util.parsing.combinator.RegexParsers
import ftanml.objects.FtanValue
import ftanml.streams.Acceptor
import ftanml.objects.FtanString
import ftanml.objects.FtanArray
import ftanml.objects.FtanNumber
import ftanml.objects.FtanBoolean
import scala.util.parsing.input.CharSequenceReader
import scala.collection.mutable.LinkedHashMap

class FtanAcceptorParser(acceptor: Acceptor) extends RegexParsers {
  
  def _null =
    "null" ^^ {_=>acceptor.processNull}

  def boolean =
    ("true" | "false") ^^ {
      value => acceptor.processBoolean(value.toBoolean)
    }

  def number =
    """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r ^^ {
      value => acceptor.processNumber(value.toDouble)
    }

  private def escapedCharacter: Parser[String] =
    "\\" ~> """[bfnrt<>"'\\/]|u[a-fA-F0-9]{4}|x[a-fA-F0-9]+;|\[(.).*?\1\]""".r ^^ {
      value => FtanString.deescapeChar("\\" + value)
    }

  private def _string: Parser[String] = {
    def stringcontent(usedQuote: Char): Parser[String] = {
      def stringCharacter: Parser[Char] =
        ("[^\\" + usedQuote + "\b\f\n\r\t]").r ^^ { _.charAt(0) }
      
      ((escapedCharacter | stringCharacter)*) ^^ {
        value => ("" /: value)(_ + _)
      }
    }
    def quoted_string(quote: Char) = quote ~> stringcontent(quote) <~ quote

    (quoted_string('"') | quoted_string('\''))
  }
  
  def string = _string ^^ {acceptor processString _}

  def array: Parser[Unit] = {
    val startArray = "[" ^^ {_=>acceptor.processStartArray}
    val endArray = "]" ^^ {_=>acceptor.processEndArray} 
    
    startArray ~> (repsep(value, ",")) <~ endArray ^^ {_=>}
  }

  def element: Parser[Unit] = {
    def attributes = {
      def nameWithoutQuotes: Parser[String] = FtanElement.VALID_NAME
      def name = (nameWithoutQuotes|_string)
      def pair = {
        def pairKey =  (name~"=") ^^ {case name~equals=>acceptor.processAttributeName(name)}
        pairKey ~ value ^^ {_=>}
      }
      def firstpair =
        (pair | name) ^^ {
          case name: String => acceptor.processStartElement(Some(name))
          case a: Unit => acceptor.processStartElement(None)
        }
      firstpair ~ (pair*)
    }
    def content = {
      def contentstringCharacter: Parser[Char] =
        """[^\\<>]""".r ^^ { _.charAt(0) }
      def contentstring =
        ((escapedCharacter | contentstringCharacter)+) ^^ {
          value => acceptor.processString(("" /: value)(_ + _))
        }
      
      ((contentstring | element)*)
    }
    
    val startAndAttributes = ("<" ~> (attributes?)) ^^ {_.getOrElse{acceptor.processStartElement(None)}}

    startAndAttributes ~ (("|" ~> content)?) <~ ">" ^^ {_=>}
  }

  def value: Parser[Unit] =
    _null | boolean | number | string | array | element

  def parse(exp : String) = {
    this.value(new CharSequenceReader(exp)) match {
      case Success(result, rest) =>
        println("Parsed correctly")
      case Failure(msg, rest) =>
        println("FAILURE: " + msg)
        FtanNull
      case Error(msg, rest) =>
        println("ERROR: " + msg)
        FtanNull
    }
  }
}