package objects

import ftanml.objects.FtanString
import org.scalatest.WordSpec
import ftanml.objects.FtanElement
import ftanml.objects.FtanList
import util.ParserTest

class StringTest extends ParserTest with WordSpec {

  "Strings" should {
    "be compared correctly (equals, hashCode)" in {

      FtanString("") should_equal FtanString("")
      FtanString("blabla") should_equal FtanString("blabla")

      FtanString("") should_not_equal FtanString("bla")
      FtanString("x") should_not_equal FtanString("x ")
      FtanString("bla") should_not_equal FtanString("bl")
      FtanString("bla") should_not_equal FtanString("la")
    }
  }

  "Strings2" should {
    "be parsed correctly" in {
      "\"bla\"" <--> FtanString("bla")
      "\"blu'\"" <--> FtanString("blu'")
      """"<>|\"'\\\b\f\n\r\t€"""" <--> FtanString("<>|\"'\\\b\f\n\r\t€")
    }
  }


  "Strings3" should {
    "escape correctly, when used in content area" in {
      def createContentElement(content: String) = FtanElement().setContent(FtanString(content))
      """<"bla">""" <--> createContentElement("bla")

    }
  }

//  "Strings4" should {
//    "escape correctly, when used as a tag name" in {
//      def createNamedElement(name: String) = FtanElement(name)
//      "<bla>" <--> createNamedElement("bla")
//      //Test that quotes are added automatically, when using a character that isn't allowed
//      "<'bla\"'>" <--> createNamedElement("bla\"")
//      "<\"bla'\">" <--> createNamedElement("bla'")
//      "<\"bla€\">" <--> createNamedElement("bla€")
//      "<b5lA_:>" <--> createNamedElement("b5lA_:")
//      //Test escaping
//      "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\">" <--> createNamedElement("<>|\"'\\\b\f\n\r\t€")
//      //Test characters that may be escaped optionally
//      "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\\/\\'\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC\">" --> createNamedElement("<>|\"'\\\b\f\n\r\t€/'><€€€€") --> "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€/'><€€€€\">"
//      "<'<>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC'>" --> createNamedElement("<>|'\"\\\b\f\n\r\t€/\"><€€€€") --> "<'<>|\\'\"\\\\\\b\\f\\n\\r\\t€/\"><€€€€'>"
//      "<'\\<\\>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC\\''>" --> createNamedElement("<>|'\"\\\b\f\n\r\t€/\"><€€€€'") --> "<\"<>|'\\\"\\\\\\b\\f\\n\\r\\t€/\\\"><€€€€'\">"
//    }
//  }

  "Strings5" should {
    "have a default value when used directly" in {
      FtanString should_equal FtanString("")
    }
  }

   "Strings6" should {
    "compare correctly, when parsed" in {
      TestParser.parsing("\"abc\"") should_equal TestParser.parsing("\"abc\"")
      TestParser.parsing("\"zbc\\[*A*]\"") should_equal TestParser.parsing("\"zbcA\"")
      TestParser.parsing(""""€"""") should_equal TestParser.parsing(""""\x20ac;"""")
      TestParser.parsing(""""€"""") should_equal TestParser.parsing(""""\x20AC;"""")
      TestParser.parsing(""""€"""") should_equal TestParser.parsing(""""\x00020ac;"""")
      TestParser.parsing("""<"ble\<">""") should_equal TestParser.parsing("""<"ble<">""")

      TestParser.parsing("\"abw\"") should_not_equal TestParser.parsing("\"abW\"")
      TestParser.parsing("\"abx\"") should_not_equal TestParser.parsing("\"abx \"")
      TestParser.parsing("\"aby\"") should_not_equal TestParser.parsing("'aby'")
    }
  }

  "Strings7" should {
    "be rejected, if wrong" in {
      """"123\"""" invalid;
    }
  }

}