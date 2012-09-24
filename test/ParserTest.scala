package test

import ftanml.FtanParser
import scala.util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import ftanml.objects.FtanValue
import org.scalatest.Suite

trait ParserTest extends ShouldMatchers {

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
  
  //Allow the should_equal matcher
  //This matcher extends the should equal matchers from ScalaTest by checking
  //lhs.equals(rhs) && rhs.equals(lhs) && lhs.hashCode==rhs.hashCode
  implicit def any2ShouldEqual[T](lhs: T) = new {
    def should_equal[U](rhs: U) {
      lhs should equal (rhs)
      rhs should equal (lhs)
      lhs.hashCode should equal (rhs.hashCode)
    }
    def should_not_equal[U](rhs: U) {
      lhs should not equal (rhs)
      rhs should not equal (lhs)
    }
  }

  //Allow the --> <-- <--> and "invalid" notation
  implicit def str2Test(str: String) = new {
    def -->(value: FtanValue) = {
      TestParser.parsing(str) should_equal value
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
      for(input <- inputs)
        input --> value
    }
  }
}