package objects

import org.scalatest.WordSpec
import util.ParserTest
import ftanml.objects._

class TextTest extends ParserTest with WordSpec {

  "Text" should {
    "be compared correctly (equals, hashCode)" in {

      FtanText("") should_equal FtanText("")
      FtanText("blabla") should_equal FtanText("blabla")
      FtanText(FtanElement("abc", "att"->FtanNumber(3))) should_equal FtanText(FtanElement("abc", "att"->FtanNumber(3)))
      FtanText(List(FtanString("abc"), FtanElement("br"), FtanString("xyz"))) should_equal FtanText(List(FtanString("abc"), FtanElement("br"), FtanString("xyz")))
      FtanText("") should_equal FtanText(List())

      FtanText("abc") should_not_equal FtanString("abc")
      FtanText("abc") should_not_equal FtanElement("abc")
      FtanText("abc") should_not_equal FtanText(FtanElement("abc"))
      FtanText("") should_not_equal FtanText("bla")
      FtanText("bla") should_not_equal FtanText("bl")
      FtanText("bla") should_not_equal FtanText("la")
    }
  }

  "Text2" should {
    "be parsed correctly" in {
      "'bla'" <-- FtanText("bla") <-- ("'bla'")
      "'<e>'" <-- FtanText(FtanElement("e")) <-- "'<e>'"
      "'<f>'" <--> FtanText(FtanElement("f"))
      """'bla"'""" <--> FtanText("bla\"")
    }
  }

//    "allow hex escapes of any length" in {
//      "\"\\xa;\"" <-- FtanString("\n") <-- ("\"\n\"", "'\n'")
//      "\"\\x00A;\"" <-- FtanString("\n")
//      "\"a\\x09;b\"" <-- FtanString("a\tb")
//      "\"\\x0041;BC\"" <-- FtanString("ABC")
//      "\"\\x00041;BC\"" <-- FtanString("ABC")
//      "\"\\x206d6;\"" <-- FtanString("\\uD869\\uDED6")
//    }

  "Text3" should {
    "escape correctly, when used in content area" in {
      def createContentElement(content: String) = FtanElement(FtanElement.CONTENT_KEY -> FtanText(content))
      "<'bla'>" <--> createContentElement("bla")
      "<'ble\\<'>" <--> createContentElement("ble<")
      "<'blu\\<\\\\\\>'>" <--> createContentElement("blu<\\>")
    }
  }



  "Text4" should {
    "compare correctly, when parsed" in {
      TestParser.parsing("'abc'") should_equal TestParser.parsing("'abc'")
      TestParser.parsing("'bbc\"'") should_equal TestParser.parsing("'bbc\\\"'")
      TestParser.parsing("'cbc '") should_equal TestParser.parsing("'cbc\\x20;'")
      TestParser.parsing("'dbc\t'") should_equal TestParser.parsing("'dbc\\t'")
      TestParser.parsing("'<e>'") should_equal TestParser.parsing("'<e>'")
      TestParser.parsing("'<f>'") should_equal TestParser.parsing("'<f >'")
      TestParser.parsing("'< g >abc< g >'") should_equal TestParser.parsing("'<g>abc<g>'")
      TestParser.parsing("'<h  a = 12 >abc'") should_equal TestParser.parsing("'<h a=12>abc'")

      TestParser.parsing("'abc'") should_not_equal TestParser.parsing("'abc '")
      TestParser.parsing("' abc'") should_not_equal TestParser.parsing("'abc '")
      TestParser.parsing("'abc<d>'") should_not_equal TestParser.parsing("'abc <d>'")
      TestParser.parsing("'abc<d>'") should_not_equal TestParser.parsing("'abc<e>'")
    }
  }

  "Text5" should {
    "have a default value when used directly" in {
      FtanText should_equal FtanText("")
    }
  }

  "Text6" should {
    "be rejected, if wrong" in {
      //TODO
    }
  }

}