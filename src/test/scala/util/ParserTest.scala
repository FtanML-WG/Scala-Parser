package util

import java.io.StringWriter

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

import org.scalatest.Suite

import ftanml.FtanAcceptorParser
import ftanml.FtanParser
import ftanml.objects.FtanValue
import ftanml.streams.Builder
import ftanml.streams.Serializer

/**
 * Inheriting from this class allows a very easy way import objects.TestHelper
 * to write test cases.
 *
 * You can just write test cases the following way:
 *
 *  "\"bla\"" --> FtanString("bla") //The parser should parse "\"bla\"" as a FtanString("bla")
 *  "\"bla\"" <-- FtanString("bla") //FtanString("bla").toFtanML should return "\"bla\""
 *  "\"bla\"" <--> FtanString("bla") //Both above should be the case
 *
 *  FtanString("bla") --> "\"bla\"" //FtanString("bla").toFtanML should return "\"bla\""
 *  FtanString("bla") <-- "\"bla\"" //The parser should parse "\"bla\"" as a FtanString("bla")
 *
 *  You can also combine them, for example
 *
 *  "\"bla\"" <-- FtanString("bla") <-- "'bla'" //The parser should parse "'bla'" as FtanString("bla") and when we output this FtanString("bla"), it should become "\"bla\""
 *
 *  When using the left-to-right syntax, we can also use tuples
 *
 *  "\"bla\"" <-- FtanString("bla") <-- ("'bla'","\"bla\"") //The parser should parse both "'bla'" and "\"bla\"" as FtanString("bla") and when we output this FtanString("bla"), it should become "\"bla\""
 *
 *  The last example could also be written as
 *
 *  "\"bla\"" <--> FtanString("bla") <-- "'bla'"
 *
 *
 *  If you need some other parser function, you can address the TestParser object directly.
 *
 *  evaluating(TestParser.number("\"bla\"")) should produce[Exception]
 *
 */
trait ParserTest extends TestHelper {

  //This trait can only be mixed into test suites
  self: Suite =>

  //Instantiate the parser
  object TestParser extends FtanParser {
    //Add an easy method for parsing a string
    def parsing(s: String): FtanValue = {
      //The phrase parser wrapper makes sure, that the whole input string is consumed
      //(so "truexyz" isn't parsed correctly as "true", because there is xyz left)
      val phraseParser = phrase(value)
      //Do the parsing
      val input = new CharSequenceReader(s)
      phraseParser(input) match {
        case Success(t, _) => t
        case NoSuccess(msg, _) => throw new IllegalArgumentException(
          "Could not parse '" + s + "': " + msg)
      }
    }
  }

  //Instantiate the acceptor parser
  object TestAcceptorParser {
    def parsing(s: String): FtanValue = {
      val builder = new Builder
      val parser = new FtanAcceptorParser(builder)
      val phraseParser = parser.phrase(parser.value)
      val input = new CharSequenceReader(s)
      phraseParser(input) match {
        case parser.Success(t, _) => builder.value
        case parser.NoSuccess(msg, _) => throw new IllegalArgumentException(
          "Could not parse '" + s + "': " + msg)
      }
    }
  }

  //Allow the --> <-- <--> and "invalid" notation
  implicit def str2Test(str: String) = new {
    def -->(value: FtanValue) = {
      TestParser.parsing(str) should_equal value
      TestAcceptorParser.parsing(str) should_equal value
      value
    }
    def <--(value: FtanValue) = {
      value.toFtanML should_equal str
      value
    }
    def <-->(value: FtanValue) = {
      val parsed = TestParser.parsing(str)
      parsed should_equal value
      val generated = parsed.toFtanML
      generated should_equal value.toFtanML
      generated should_equal str
      //Test AcceptorParser
      TestAcceptorParser.parsing(str) should_equal parsed

      value
    }
    def invalid {
      evaluating(TestParser.parsing(str)) should produce[IllegalArgumentException]
    }
  }
  implicit def val2Test(value: FtanValue) = new {
    def -->(str: String) = {
      str <-- value
      str
    }
    def <--(str: String) = {
      str --> value
      str
    }
    def <-->(str: String) = {
      str <--> value
      str
    }
    def <--(inputs: String*) = {
      for (input <- inputs)
        input --> value
    }
  }
}