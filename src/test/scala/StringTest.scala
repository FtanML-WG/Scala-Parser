import ftanml.objects.FtanString
import org.scalatest.WordSpec
import ftanml.objects.FtanElement
import ftanml.objects.FtanArray

class StringTest extends ParserTest with WordSpec {

  "Strings" should {
    "be compared correctly (equals, hashCode)" in {

      FtanString("") should_equal FtanString("")
      FtanString("blabla") should_equal FtanString("blabla")

      FtanString("") should_not_equal FtanString("bla")
      FtanString("bla") should_not_equal FtanString("bl")
      FtanString("bla") should_not_equal FtanString("la")
    }

    "be parsed correctly" in {
      "\"bla\"" <-- FtanString("bla") <-- ("\"bla\"", "'bla'")
      "'bla\"'" <-- FtanString("bla\"") <-- ("'bla\"'", "\"bla\\\"\"", "'bla\\\"'")
      "\"bla'\"" <-- FtanString("bla'") <-- ("\"bla'\"", "'bla\\''", "\"bla\\'\"")
      "\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\"" <--> FtanString("<>|\"'\\\b\f\n\r\t€")
      "\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€/'><€€€€\"" <-- FtanString("<>|\"'\\\b\f\n\r\t€/'><€€€€") <-- "\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\\/\\'\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC\""
      "'<>|\\'\"\\\\\\b\\f\\n\\r\\t€/\"><€€€€'" <-- FtanString("<>|'\"\\\b\f\n\r\t€/\"><€€€€") <-- "'<>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC'"
    }

//    "allow hex escapes of any length" in {
//      "\"\\xa;\"" <-- FtanString("\n") <-- ("\"\n\"", "'\n'")
//      "\"\\x00A;\"" <-- FtanString("\n")
//      "\"a\\x09;b\"" <-- FtanString("a\tb")
//      "\"\\x0041;BC\"" <-- FtanString("ABC")
//      "\"\\x00041;BC\"" <-- FtanString("ABC")
//      "\"\\x206d6;\"" <-- FtanString("\\uD869\\uDED6")
//    }

    "escape correctly, when used in content area" in {
      def createContentElement(content: String) = FtanElement(FtanElement.CONTENT_KEY -> FtanArray(Seq(FtanString(content))))
      "<|bla>" <--> createContentElement("bla")
      "<|bla\\<\\\\\\>>" <--> createContentElement("bla<\\>")
      "<|\\<\\>|'\"\\\\\b\f\n\r\t€/\"\\>\\<€€€€'>" <-- createContentElement("<>|'\"\\\b\f\n\r\t€/\"><€€€€'") <-- "<|\\<\\>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC'>"
//      "<|\\u0041>" <-- createContentElement("A")
//      "<|\\x0041;>" <-- createContentElement("A")
    }

    "escape correctly, when used as a tag name" in {
      def createNamedElement(name: String) = FtanElement(FtanElement.NAME_KEY -> FtanString(name))
      "<bla>" <--> createNamedElement("bla")
      //Test that quotes are added automatically, when using a character that isn't allowed
      "<'bla\"'>" <--> createNamedElement("bla\"")
      "<\"bla'\">" <--> createNamedElement("bla'")
      "<\"bla€\">" <--> createNamedElement("bla€")
      "<b5lA_:>" <--> createNamedElement("b5lA_:")
      //Test escaping
      "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\">" <--> createNamedElement("<>|\"'\\\b\f\n\r\t€")
      //Test characters that may be escaped optionally
      "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€\\/\\'\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC\">" --> createNamedElement("<>|\"'\\\b\f\n\r\t€/'><€€€€") --> "<\"<>|\\\"'\\\\\\b\\f\\n\\r\\t€/'><€€€€\">"
      "<'<>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC'>" --> createNamedElement("<>|'\"\\\b\f\n\r\t€/\"><€€€€") --> "<'<>|\\'\"\\\\\\b\\f\\n\\r\\t€/\"><€€€€'>"
      "<'\\<\\>|\\\'\"\\\\\\b\\f\\n\\r\\t€\\/\\\"\\>\\<\\u20ac\\u20aC\\u20Ac\\u20AC\\''>" --> createNamedElement("<>|'\"\\\b\f\n\r\t€/\"><€€€€'") --> "<\"<>|'\\\"\\\\\\b\\f\\n\\r\\t€/\\\"><€€€€'\">"
    }

    "have a default value when used directly" in {
      FtanString should_equal FtanString("")
    }

    "be rejected, if wrong" in {
      //TODO
    }
  }

}