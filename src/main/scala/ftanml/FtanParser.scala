package ftanml

import expr._
import functions._
import objects._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

class FtanParser extends RegexParsers with DebugParser {

  override val skipWhitespace = false

  /**
   * Optional whitespace
   */
  def s: Parser[Option[String]] = {
     "[ \n\r\t]*".r  ^^ {
      case s => None
    }
  }

  /**
   * A keyword preceded by optional whitespace
   */
  def k(keyword: String): Parser[String] = {
    s ~> keyword
  }

  def _null: Parser[FtanNull.type] =
    k("null") ^^^ (FtanNull)

  def boolean: Parser[FtanBoolean] =
    (k("true") | k("false")) ^^ {
      value => FtanBoolean(value.toBoolean)
    }

  def number: Parser[FtanNumber] =
    s ~> """-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r ^^ {
      value => FtanNumber(value)
    }

  private def escapedCharacter: Parser[String] =
    "\\" ~> """[bfnrt<>"'\\/]|u[a-fA-F0-9]{4}|x[a-fA-F0-9]+;|\[(.).*?\1\]""".r ^^ {
      value => FtanString.unescapeString("\\" + value)
    }

  def array: Parser[FtanList] = {
    k("[") ~> ( rep(value | k(",")) ) <~ k("]") ^^ {
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

  def string: Parser[FtanString] = {
     k("\"") ~> escapeRegex("\"") <~ "\""  ^^ {
      case s => FtanString(FtanString.unescapeString(s))
    }
  }

  def backTickName: Parser[String] = {
    k("`") ~> escapeRegex("`") <~ "`"  ^^ {
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
        (s ~> name) ~ k("=") ~ value ^^ {
          case name ~ sep ~ value => (name, value)
        }

    def attributes: Parser[Map[String, FtanValue]] = rep(pair) ^^ {
      case p => Map() ++ p
    }

    "<" ~> (s ~> opt(initial)) ~ (s ~> attributes) ~ opt(value) <~ k(">") ^^ {
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

    k("'") ~> ((contentstring | element)*) <~ "'" ^^ {
      value => FtanText(value)
    }

  }

  def literal: Parser[Expression] = {
    value ^^ {
      v => new Literal(v)
    }
  }

  def parenthetical: Parser[Expression] = {
    k("(") ~> expression <~ k(")")
  }

  def argumentRef: Parser[Expression] = {
    s ~> "_[0-9]*".r ^^ {
      case ref => {
        if (ref.length == 1) {
          new ParamRef(1)
        } else {
          new ParamRef(Integer.parseInt(ref.substring(1)))
        }
      }
    }
  }

  def primary: Parser[Expression] = {
    literal | parenthetical | argumentRef
  }

  def attributeExpr: Parser[Expression] = {
    primary ~ rep( k("@") ~> (name | parenthetical)) ^^ {
      case base ~ path => {
        (base /: path)((b: Expression,  step: AnyRef) => step match {
          case name: String => new FunctionCall(ftanml.functions.attribute, b::Literal(FtanString(name))::Nil)
          case expr: Expression => new FunctionCall(ftanml.functions.attribute, b::expr::Nil)
        })
      }
    }
  }

  def subscript: Parser[Expression => Expression] = {
    k("[") ~> expression <~ k("]") ^^ {
      case sub => {
        { (base: Expression) => new FunctionCall(ftanml.functions.subscript, base::sub::Nil)}
      }
    }
  }

  def argumentList: Parser[Expression => Expression] = {
    k("(") ~> repsep(expression, k(",")) <~ k(")") ^^ {
      case args => {
        { (base: Expression) => new FunctionCall(base :: args)}
      }
    }
  }

  def postfixExpr: Parser[Expression] = {
    attributeExpr ~ rep ( subscript | argumentList ) ^^ {
      case base ~ suffixes => {
        (base /: suffixes)((b:Expression, f:Expression=>Expression) => f(b))
      }
    }
  }

  def applicativeExpr: Parser[Expression] = {
    postfixExpr * (
            k("!") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.map, a::b::Nil) } |
            k("?") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.filter, a::b::Nil) } )
  }

  def multiplicativeExpr: Parser[Expression] = {
    applicativeExpr * (
            k("×") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.times, a::b::Nil) } |
            k("÷") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.div, a::b::Nil) } )
  }

  def additiveExpr: Parser[Expression] = {
    multiplicativeExpr * (
            k("+") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.plus, a::b::Nil) } |
            k("-") ^^^ { (a:Expression, b:Expression) => new FunctionCall(ftanml.functions.minus, a::b::Nil) } )
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
   comparisonExpr ~ opt(k("&&") ~> andExpr) ^^ {
      case lhs ~ rhs => {
        rhs match {
          case Some(e) => new AndExpr(lhs, e)
          case None => lhs
        }
      }
    }
  }

  def expression: Parser[Expression] = {
    andExpr ~ opt(k("||") ~> expression) ^^ {
      case lhs ~ rhs => {
        rhs match {
          case Some(e) => new OrExpr(lhs, e)
          case None => lhs
        }
      }
    }
  }

  def function: Parser[UserFunction] = {
    k("{") ~> expression <~ k("}") ^^ {
      e => new UserFunction(e)
    }
  }

  def value: Parser[FtanValue] =
    _null | boolean | number | string | array | (s ~> element) | text | function

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
