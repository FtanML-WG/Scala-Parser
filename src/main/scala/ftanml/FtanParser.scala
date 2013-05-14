package ftanml

import expr._
import functions._
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
      value => FtanNumber(value)
    }

  private def escapedCharacter: Parser[String] =
    "\\" ~> """[bfnrt<>"'\\/]|u[a-fA-F0-9]{4}|x[a-fA-F0-9]+;|\[(.).*?\1\]""".r ^^ {
      value => FtanString.unescapeString("\\" + value)
    }

//  def array: Parser[FtanArray] =
//    s ~> "[" ~> s ~> (repsep((s ~> value <~ s), ",")) <~ "]" <~ s ^^ {
//      value => FtanArray(value)
//    }

  def array: Parser[FtanList] = {

    s ~> "[" ~> ( rep(s ~> (value | ",")) <~ s) <~ ("]" ~ s) ^^ {
      case Nil => FtanList()
      case seq => {
        FtanList(
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

  def literal: Parser[Expression] = {
    s ~> value ^^ {
      v => new Literal(v)
    }
  }

  def parenthetical: Parser[Expression] = {
    (s ~ "(" ~ s) ~> expression <~ (s ~ ")" ~ s) ^^ {e => e}
  }

  def primary: Parser[Expression] = {
    literal | parenthetical
  }

  def multiplicativeExpr: Parser[Expression] = {
    primary * (
            (s ~> "×" <~ s) ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.times, a::b::Nil) } |
            (s ~> "÷" <~ s) ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.div, a::b::Nil) } )
  }

  def additiveExpr: Parser[Expression] = {
    multiplicativeExpr * (
            (s ~> "+" <~ s) ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.plus, a::b::Nil) } |
            (s ~> "-" <~ s) ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.minus, a::b::Nil) } )
  }

  def comparisonExpr: Parser[Expression] = {
    additiveExpr ~ opt(s ~> ("="|"!="|"<="|"<"|">="|">") ~ additiveExpr) ^^ {
      case l ~ pred => {
        pred match {
          case Some(op~r) => {
            op match {
              case "=" => new FunctionCall(ftanml.functions.eq, l::r::Nil)
              case "!=" => new FunctionCall(ftanml.functions.ne, l::r::Nil)
              case "<=" => new FunctionCall(ftanml.functions.le, l::r::Nil)
              case "<" => new FunctionCall(ftanml.functions.lt, l::r::Nil)
              case ">=" => new FunctionCall(ftanml.functions.ge, l::r::Nil)
              case ">" => new FunctionCall(ftanml.functions.gt, l::r::Nil)
            }
          }
          case None => l
        }
      }
    }
  }

  def andExpr: Parser[Expression] = {
   comparisonExpr ~ opt(s ~> "&&" ~> andExpr) ^^ {
      case lhs ~ rhs => {
        rhs match {
          case Some(e) => new AndExpr(lhs, e)
          case None => lhs
        }
      }
    }
  }

  def expression: Parser[Expression] = {
    andExpr ~ opt(s ~> "||" ~> expression) ^^ {
      case lhs ~ rhs => {
        rhs match {
          case Some(e) => new OrExpr(lhs, e)
          case None => lhs
        }
      }
    }
  }

  def function: Parser[UserFunction] = {
    s ~> "{" ~> expression <~ ("}" ~ s) ^^ {
      e => new UserFunction(e)
    }
  }

  def value: Parser[FtanValue] =
    _null | boolean | number | string | array | element | text | function

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
