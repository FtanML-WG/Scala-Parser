package objects

import org.scalatest.FlatSpec
import util.ParserTest
import ftanml.objects._

class ElementTest extends ParserTest with FlatSpec {
  "Elements" should "be compared correctly (equals, hashCode)" in {
    FtanElement() should_equal FtanElement()
    FtanElement("bla" -> FtanString("value")) should_equal FtanElement("bla" -> FtanString("value"))
    FtanElement("key" -> FtanNumber(2.34)) should_equal FtanElement("key" -> FtanNumber(2.34))
    FtanElement("key" -> FtanTrue, "key2" -> FtanNull) should_equal FtanElement("key" -> FtanBoolean(true), "key2" -> FtanNull)
    FtanElement("key" -> FtanNull) should_equal FtanElement("key" -> FtanNull)
    FtanElement("key2" -> FtanNull, "key3" -> FtanString("MeinName"), "key4" -> FtanString("bla")) should_equal FtanElement("key2" -> FtanNull, "key3" -> FtanString("MeinName"), "key4" -> FtanString("bla"))
    FtanElement("MeinName", "key" -> FtanBoolean(true), "key2" -> FtanNull, "" -> FtanArray(FtanString("before"), FtanElement("bla" -> FtanArray()), FtanString("after"))) should_equal FtanElement("MeinName", "key" -> FtanBoolean(true), "key2" -> FtanNull, FtanElement.CONTENT_KEY -> FtanArray(FtanString("before"), FtanElement("bla" -> FtanArray()), FtanString("after")))
    FtanElement("bla" -> FtanBoolean(true)) should_not_equal FtanElement("bla" -> FtanBoolean(false))
    FtanElement("bla" -> FtanBoolean(true)) should_not_equal FtanElement("blb" -> FtanBoolean(true))
    FtanElement("bla" -> FtanTrue, "bla2" -> FtanNull) should_equal FtanElement("bla" -> FtanTrue) //sic
    FtanElement("bla" -> FtanString("1")) should_not_equal FtanElement("bla" -> FtanNumber(1))
    FtanElement("bla" -> FtanString("1.0")) should_not_equal FtanElement("bla" -> FtanNumber(1))
    FtanElement("foo" -> FtanString("1.0")) should_not_equal FtanElement("bar" -> FtanString("1.0"))
    FtanElement("foo") should_not_equal  FtanElement("bar")
  }

  "Elements2" should "ignore the attribute order when comparing (equals, hashCode)" in {
    FtanElement("key1" -> FtanNumber(1), "key2" -> FtanNumber(2)) should_equal FtanElement("key2" -> FtanNumber(2), "key1" -> FtanNumber(1))

    val map1 = Map[String, FtanValue]("key1" -> FtanNumber(1), "key2" -> FtanNumber(2))
    val map2 = Map[String, FtanValue]("key2" -> FtanNumber(2), "key1" -> FtanNumber(1))
    new FtanElement(None, map1) should_equal new FtanElement(None, map2)
  }

  "Elements3" should "pass simple parser tests" in {
    "<>" <--> FtanElement()
    "<``>" <--> FtanElement("")
    "<`!!`>" <--> FtanElement("!!")
    "<`\\x20ac;`>" --> FtanElement("€")
    "<myTagName>" <--> FtanElement("myTagName")
    "<'myContent'>" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanText("myContent"))
    "<int 23>" <--> FtanElement("int", FtanElement.CONTENT_KEY -> FtanNumber(23))
    "<str \"myContent\">" <--> FtanElement("str", FtanElement.CONTENT_KEY -> FtanString("myContent"))
    "<bool false>" <--> FtanElement("bool", FtanElement.CONTENT_KEY -> FtanFalse)
    "<array [1,2,3]>" <--> FtanElement("array", FtanElement.CONTENT_KEY -> FtanArray(FtanNumber(1), FtanNumber(2), FtanNumber(3)))
  }

  "Elements4" should "parse attributes correctly" in {
    "<attr=\"myValue\">" <-- FtanElement("attr" -> FtanString("myValue")) <-- (
      "<attr=\"myValue\">", "<`attr`=\"myValue\">")
    "<tagname attr=\"myValue\">" <--
      FtanElement("tagname", "attr" -> FtanString("myValue")) <-- (
        "<tagname attr=\"myValue\">", "<tagname `attr`=\"myValue\">",
        "<`tagname` attr=\"myValue\">", "<`tagname` `attr`=\"myValue\">")
    "<attr1=false attr2=2>" <--> FtanElement("attr1" -> FtanFalse, "attr2" -> FtanNumber(2.0))
  }

  "Elements5" should "work when used with arrays" in {
    "<attr=[null,<'bla<a=[[]]>'>]>" <--> FtanElement("attr" -> FtanArray(FtanNull, FtanElement(FtanElement.CONTENT_KEY -> FtanText(FtanString("bla"), FtanElement("a" -> FtanArray(FtanArray()))))))
    "[\"bla\",<attr=[null,<'bla<a=[[]]>'>]>,\"bla2\"]" <--> FtanArray(FtanString("bla"), FtanElement("attr" -> FtanArray(FtanNull, FtanElement(FtanElement.CONTENT_KEY -> FtanText(FtanString("bla"), FtanElement("a" -> FtanArray(FtanArray())))))), FtanString("bla2"))
  }

  "Elements6" should "handle tag names correctly, when containing invalid characters" in {
    "<`att'r1`=\"myValue\">" <-- FtanElement("att'r1" -> FtanString("myValue")) <-- (
      "<`att'r1`=\"myValue\">", "<`att\\'r1`=\"myValue\">")
    "<`att\"r3`=\"myValue\">" <-- FtanElement("att\"r3" -> FtanString("myValue")) <-- (
      "<`att\"r3`=\"myValue\">", "<`att\\\"r3`=\"myValue\">")
    "<`€`>" <--> FtanElement("€")
    "<`.2`>" <--> FtanElement(".2")

    "<a123:4_5 'myContent<innerName>afterContent'>" <--> FtanElement("a123:4_5", FtanElement.CONTENT_KEY -> FtanText(FtanString("myContent"), FtanElement("innerName"), FtanString("afterContent")))
    "<`.12` 'myContent<innerName>afterContent'>" <--> FtanElement(".12", FtanElement.CONTENT_KEY -> FtanText(FtanString("myContent"), FtanElement("innerName"), FtanString("afterContent")))
    "<`1.2` 'myContent<innerName>afterContent'>" <--> FtanElement("1.2", FtanElement.CONTENT_KEY -> FtanText(FtanString("myContent"), FtanElement("innerName"), FtanString("afterContent")))
  }

  "Elements7" should "handle nested content tags correctly" in {
    "<`1.2` attr1=1.3 attr2=false 'myContent<innerName>afterContent'>" <--> FtanElement("1.2", "attr1" -> FtanNumber(1.3), "attr2" -> FtanFalse, FtanElement.CONTENT_KEY -> FtanText(FtanString("myContent"), FtanElement("innerName"), FtanString("afterContent")))
    "<a '<b><c '<d>'>'>" <--> FtanElement("a", "" -> FtanText(FtanElement("b"), FtanElement("c", "" -> FtanText(FtanElement("d")))))
  }
  // TODO: reinstate below tests when the spec is clearer
//
//  "Elements8" should "output invalid names as an attribute" in {
//    "<$name=3>" <-- FtanElement(FtanElement.NAME_KEY -> FtanNumber(3)) <-- ("<$name=3.0>", "<$name=3>")
//    "<$name=null>" <--> FtanElement(FtanElement.NAME_KEY -> FtanNull)
//    "<$name=[\"bla\"]>" <--> FtanElement(FtanElement.NAME_KEY -> FtanArray(FtanString("bla")))
//    "<$name=<bla>>" <--> FtanElement(FtanElement.NAME_KEY -> FtanElement(FtanElement.NAME_KEY -> FtanString("bla")))
//    "<$name=1.2|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanNumber(1.2), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
//  }
//
//  "Elements9" should "output invalid content as an attribute" in {
//    "<$content=\"bla\">" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanString("bla"))
//    "<$content=[\"bla\",0.0]>" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanArray(FtanString("bla"), FtanNumber(0)))
//    "<a1:2 $content=[\"myContent\",<innerName>,1.0,\"afterContent\"]>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("a1:2"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanNumber(1.0), FtanString("afterContent")))
//  }

  "Elements10" should "preserve the attribute order" in {
    "<a=1 b=2>" <--> FtanElement("a" -> FtanNumber(1.0), "b" -> FtanNumber(2.0))
    "<a=2 b=1>" <--> FtanElement("a" -> FtanNumber(2.0), "b" -> FtanNumber(1.0))
    "<b=1 a=2>" <--> FtanElement("b" -> FtanNumber(1.0), "a" -> FtanNumber(2.0))
    "<b=2 a=1>" <--> FtanElement("b" -> FtanNumber(2.0), "a" -> FtanNumber(1.0))
  }

  "Elements11" should "have a default value when used directly" in {
    FtanElement should_equal FtanElement()
    FtanElement() should_not_equal FtanElement("")
  }

  "Elements12" should "compare correctly, when parsed" in {
    TestParser.parsing("<a>") should_equal TestParser.parsing("<`a`>")
    TestParser.parsing("<a <>>") should_equal TestParser.parsing("<`a` <>>")
    TestParser.parsing("<a=1 b=2>") should_equal TestParser.parsing("<b=2 a=1>")
    TestParser.parsing("<a=1 b=null>") should_equal TestParser.parsing("<a=1>")
    TestParser.parsing("<a null>") should_equal TestParser.parsing("<a>")
    TestParser.parsing("<e a=1 b=2 3>") should_equal TestParser.parsing("<e b=2 a=1 3>")
    TestParser.parsing("<e a=1 b=2 3>") should_equal TestParser.parsing("<e b=2.0 a=1.0 3.0>")
    TestParser.parsing("<`\\x20ac;`>") should_equal TestParser.parsing("<`€`>")
    TestParser.parsing("<`\\[*A*]`>") should_equal TestParser.parsing("<A>")

    TestParser.parsing("<a>") should_not_equal TestParser.parsing("<`a `>")
    TestParser.parsing("<b <>>") should_not_equal TestParser.parsing("<`b` [<>]>")
    TestParser.parsing("<c ''>") should_not_equal TestParser.parsing("<`c` \"\">")
  }

  "Elements13" should "be rejected, if wrong" in {
    "<.3>" invalid; //Invalid name
    //TODO
  }
}