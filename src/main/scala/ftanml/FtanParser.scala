package ftanml

import objects._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

class FtanParser extends RegexParsers with DebugParser {

  override val skipWhitespace = false

  def _null: Parser[FtanNull.type] =
    "null" ^^^ (FtanNull)

  def boolean: Parser[FtanBoolean] =
    ("true" | "false") ^^ {
      value => FtanBoolean(value.toBoolean)
    }

  def number: Parser[FtanNumber] =
    """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r ^^ {
      value => FtanNumber(value.toDouble)
    }

  private def escapedCharacter: Parser[String] =
    "\\" ~> """[bfnrt<>"'\\/]|u[a-fA-F0-9]{4}|x[a-fA-F0-9]+;|\[(.).*?\1\]""".r ^^ {
      value => FtanString.unescapeString("\\" + value)
    }

//  def array: Parser[FtanArray] =
//    s ~> "[" ~> s ~> (repsep((s ~> value <~ s), ",")) <~ "]" <~ s ^^ {
//      value => FtanArray(value)
//    }

  def array: Parser[FtanArray] = {

    s ~> "[" ~> ( rep(s ~> (value | ",")) <~ s) <~ ("]" ~ s) ^^ {
      case Nil => FtanArray()
      case seq => {
        FtanArray(
          (","+:seq, seq:+",").zipped.flatMap((X, Y) =>
            (X, Y) match {
              case (",", ",") => Seq(FtanNull)
              case (",", _) => Nil
              case (v, _) => Seq(v)
            }).asInstanceOf[Seq[FtanValue]]
        )
      }
    }
  }

  def escapeRegex(disallowed: String) =
    ("""(?:[^\\""" + disallowed + """]|\\[bfnrt<>\"'\\]|\\x[a-fA-F0-9]+;|\\\[(.).*?\1\])*""").r

  def s: Parser[Option[String]] = {
     "[ \n\r\t]*".r  ^^ {
      case s => None
    }
  }

  def string: Parser[FtanString] = {
     "\"" ~> escapeRegex("\"") <~ "\""  ^^ {
      case s => FtanString(FtanString.unescapeString(s))
    }
  }

  def backTickName: Parser[String] = {
    "`" ~> escapeRegex("`") <~ "`"  ^^ {
      case s => FtanString.unescapeString(s)
    }
  }

  def name: Parser[String] = FtanElement.VALID_NAME | backTickName

  def element : Parser[FtanElement] = {

    def firstAtt: Parser[Option[FtanValue]] =
        opt("=" ~> value)

    def initial: Parser[FtanElement] =
        (s ~> name) ~ (s ~> firstAtt) ^^ {
          case name ~ firstAtt =>
            firstAtt match {
              case Some(v) =>
                FtanElement(name -> v)
              case None =>
                FtanElement(name)
            }
        }

    def pair: Parser[(String, FtanValue)] =
        (s ~> name) ~ (s ~> "=" <~ s) ~ value ^^ {
          case name ~ sep ~ value => (name, value)
        }

    def attributes: Parser[Map[String, FtanValue]] = rep(pair) ^^ {
      case p => Map() ++ p
    }

    "<" ~> (s ~> opt(initial)) ~ (s ~> attributes) ~ (s ~> opt(value)) <~ s <~ ">" ^^ {
      case initial ~ attributes ~ content =>
        initial match {
          case Some(e) =>
            content match {
              case Some(v) => e.setAttributes(attributes.toMap).setContent(v)
              case none => e.setAttributes(attributes)
            }
          case None =>
            content match {
              case Some(v) => FtanElement(attributes.toMap).setContent(v)
              case none => FtanElement(attributes)
            }

        }

    }

  }

  def text : Parser[FtanText] = {

      def contentstringCharacter: Parser[Char] =
        "[^\\<>']".r ^^ { _.charAt(0) }

      def contentstring: Parser[FtanString] =
        ((escapedCharacter | contentstringCharacter)+) ^^ {
          value => FtanString(("" /: value)(_ + _))
        }

      "'" ~> ((contentstring | element)*) <~ "'" ^^ {
        value => FtanText(value)
      }

    }

// following code looks OK but goes into infinite loop: MHK 2013-05-09
// def text : Parser[FtanText] = {
//
//      def contentstring: Parser[FtanString] =
//        escapeRegex("'<") ^^ {
//          case s => FtanString(FtanString.deescapeString(s))
//        }
//
//      "'" ~> ((contentstring | element)*) <~ "'" ^^ {
//        value => FtanText(value)
//      }
//
//    }

  def value: Parser[FtanValue] =
    _null | boolean | number | string | array | element | text

  def parse(exp: String): FtanValue = {
    this.value(new CharSequenceReader(exp)) match {
      case Success(result, rest) =>
        result
      case Failure(msg, rest) =>
        println("FAILURE: " + msg)
        FtanNull
      case Error(msg, rest) =>
        println("ERROR: " + msg)
        FtanNull
    }
  }
}

object MyApp extends App {
  val parser = new FtanParser

  for (line <- io.Source.stdin.getLines()) {
    parser.value(new CharSequenceReader(line + "\n>")) match {
      case parser.Success(result, rest) =>
        println(result.toFtanML)
      case parser.Failure(msg, rest) =>
        println("FAILURE: " + msg)
      case parser.Error(msg, rest) =>
        println("ERROR: " + msg)
    }
  }
}
