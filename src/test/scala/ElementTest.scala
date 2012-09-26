package test

import org.scalatest.FlatSpec
import ftanml.objects.FtanElement
import ftanml.objects.FtanString
import ftanml.objects.FtanValue
import scala.collection.mutable.LinkedHashMap
import ftanml.objects.FtanNumber
import ftanml.objects.FtanBoolean
import ftanml.objects.FtanNull
import ftanml.objects.FtanArray

class ElementTest extends ParserTest with FlatSpec {
  "Elements" should "be compared correctly (equals, hashCode)" in {
    FtanElement() should_equal FtanElement()
    FtanElement(FtanString("bla") -> FtanString("value")) should_equal FtanElement(FtanString("bla") -> FtanString("value"))
    FtanElement(FtanString("key") -> FtanNumber(2.34)) should_equal FtanElement(FtanString("key") -> FtanNumber(2.34))
    FtanElement(FtanString("key") -> FtanBoolean(true), FtanString("key2") -> FtanNull) should_equal FtanElement(FtanString("key") -> FtanBoolean(true), FtanString("key2") -> FtanNull)
    FtanElement(FtanString("key") -> FtanNull) should_equal FtanElement(FtanString("key") -> FtanNull)
    FtanElement(FtanString("key2") -> FtanNull, FtanString("key3") -> FtanString("MeinName"), FtanString("key4") -> FtanString("bla")) should_equal FtanElement(FtanString("key2") -> FtanNull, FtanString("key3") -> FtanString("MeinName"), FtanString("key4") -> FtanString("bla"))
    FtanElement(FtanString("key") -> FtanBoolean(true), FtanString("key2") -> FtanNull, FtanElement.NAME_KEY -> FtanString("MeinName"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("before"), FtanElement(FtanString("bla") -> FtanArray()), FtanString("after"))) should_equal FtanElement(FtanString("key") -> FtanBoolean(true), FtanString("key2") -> FtanNull, FtanElement.NAME_KEY -> FtanString("MeinName"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("before"), FtanElement(FtanString("bla") -> FtanArray()), FtanString("after")))
    FtanElement(FtanString("bla") -> FtanBoolean(true)) should_not_equal FtanElement(FtanString("bla") -> FtanBoolean(false))
    FtanElement(FtanString("bla") -> FtanBoolean(true)) should_not_equal FtanElement(FtanString("blb") -> FtanBoolean(true))
    FtanElement(FtanString("bla") -> FtanBoolean(true), FtanString("bla2") -> FtanNull) should_not_equal FtanElement(FtanString("bla") -> FtanBoolean(true))
    FtanElement(FtanString("bla") -> FtanString("1")) should_not_equal FtanElement(FtanString("bla") -> FtanNumber(1))
    FtanElement(FtanString("bla") -> FtanString("1.0")) should_not_equal FtanElement(FtanString("bla") -> FtanNumber(1))
  }

  they should "ignore the attribute order when comparing (equals, hashCode)" in {
    FtanElement(FtanString("key1") -> FtanNumber(1), FtanString("key2") -> FtanNumber(2)) should_equal FtanElement(FtanString("key2") -> FtanNumber(2), FtanString("key1") -> FtanNumber(1))

    val map1 = new LinkedHashMap[FtanString, FtanValue]
    val map2 = new LinkedHashMap[FtanString, FtanValue]
    map1 += FtanString("key1") -> FtanNumber(1)
    map1 += FtanString("key2") -> FtanNumber(2)
    map2 += FtanString("key2") -> FtanNumber(2)
    map2 += FtanString("key1") -> FtanNumber(1)
    FtanElement(map1) should_equal FtanElement(map2)
  }

  they should "pass simple parser tests" in {
    "<>" <--> FtanElement()
    "<\"\">" <-- FtanElement(FtanElement.NAME_KEY -> FtanString("")) <-- ("<\"\">", "<''>")
    "<myTagName>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("myTagName"))
    "<|myContent>" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent")))
    "<myTagName|myContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("myTagName"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent")))

  }

  they should "parse attributes correctly" in {
    "<attr=\"myValue\">" <-- FtanElement(FtanString("attr") -> FtanString("myValue")) <-- (
      "<attr=\"myValue\">", "<\"attr\"=\"myValue\">", "<'attr'=\"myValue\">",
      "<attr='myValue'>", "<\"attr\"='myValue'>", "<'attr'='myValue'>")
    "<tagname attr=\"myValue\">" <--
      FtanElement(FtanElement.NAME_KEY -> FtanString("tagname"), FtanString("attr") -> FtanString("myValue")) <-- (
        "<tagname attr=\"myValue\">", "<tagname \"attr\"=\"myValue\">", "<tagname 'attr'=\"myValue\">",
        "<tagname attr='myValue'>", "<tagname \"attr\"='myValue'>", "<tagname 'attr'='myValue'>",
        "<'tagname' attr=\"myValue\">", "<'tagname' \"attr\"=\"myValue\">", "<'tagname' 'attr'=\"myValue\">",
        "<'tagname' attr='myValue'>", "<'tagname' \"attr\"='myValue'>", "<'tagname' 'attr'='myValue'>",
        "<\"tagname\" attr=\"myValue\">", "<\"tagname\" \"attr\"=\"myValue\">", "<\"tagname\" 'attr'=\"myValue\">",
        "<\"tagname\" attr='myValue'>", "<\"tagname\" \"attr\"='myValue'>", "<\"tagname\" 'attr'='myValue'>")
    "<attr1=false attr2=2.0>" <--> FtanElement(FtanString("attr1") -> FtanBoolean(false), FtanString("attr2") -> FtanNumber(2.0))
  }

  they should "work when used with arrays" in {
    "<attr=[null,<|bla<a=[[]]>>]>" <--> FtanElement(FtanString("attr") -> FtanArray(FtanNull, FtanElement(FtanElement.CONTENT_KEY -> FtanArray(FtanString("bla"), FtanElement(FtanString("a") -> FtanArray(FtanArray()))))))
    "[\"bla\",<attr=[null,<|bla<a=[[]]>>]>,\"bla2\"]" <--> FtanArray(FtanString("bla"), FtanElement(FtanString("attr") -> FtanArray(FtanNull, FtanElement(FtanElement.CONTENT_KEY -> FtanArray(FtanString("bla"), FtanElement(FtanString("a") -> FtanArray(FtanArray())))))), FtanString("bla2"))
  }

  they should "handle tag names correctly, when containing invalid characters" in {
    "<\"att'r\"=\"myValue\">" <-- FtanElement(FtanString("att'r") -> FtanString("myValue")) <-- (
      "<\"att'r\"=\"myValue\">", "<'att\\'r'=\"myValue\">")
    "<'att\"r'=\"myValue\">" <-- FtanElement(FtanString("att\"r") -> FtanString("myValue")) <-- (
      "<'att\"r'=\"myValue\">", "<\"att\\\"r\"=\"myValue\">")
    "<\"€\">" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("€"))
    "<\"2\">" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("2"))

    "<a123:4_5|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("a123:4_5"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
    "<\"12\"|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("12"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
    "<\"1.2\"|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("1.2"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
  }

  they should "handle nested content tags correctly" in {
    "<\"1.2\" attr1=1.3 attr2=null|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("1.2"), FtanString("attr1") -> FtanNumber(1.3), FtanString("attr2") -> FtanNull, FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
    "<outertag style=\"outer\"|start<inner|<second|<|third<|<>>>>after>>" <--> FtanElement(
      FtanElement.NAME_KEY -> FtanString("outertag"),
      FtanString("style") -> FtanString("outer"),
      FtanElement.CONTENT_KEY -> FtanArray(
        FtanString("start"),
        FtanElement(
          FtanElement.NAME_KEY -> FtanString("inner"),
          FtanElement.CONTENT_KEY -> FtanArray(
            FtanElement(
              FtanElement.NAME_KEY -> FtanString("second"),
              FtanElement.CONTENT_KEY -> FtanArray(FtanElement(
                FtanElement.CONTENT_KEY -> FtanArray(
                  FtanString("third"),
                  FtanElement(
                    FtanElement.CONTENT_KEY -> FtanArray(
                      FtanElement())))))),
            FtanString("after")))))

    "<\"outer'tag\" style=\"outer\"|start<inner|<second attr=null|<|third<|<>>>>after>>" <--> FtanElement(
      FtanElement.NAME_KEY -> FtanString("outer'tag"),
      FtanString("style") -> FtanString("outer"),
      FtanElement.CONTENT_KEY -> FtanArray(
        FtanString("start"),
        FtanElement(
          FtanElement.NAME_KEY -> FtanString("inner"),
          FtanElement.CONTENT_KEY -> FtanArray(
            FtanElement(
              FtanElement.NAME_KEY -> FtanString("second"),
              FtanString("attr") -> FtanNull,
              FtanElement.CONTENT_KEY -> FtanArray(FtanElement(
                FtanElement.CONTENT_KEY -> FtanArray(
                  FtanString("third"),
                  FtanElement(
                    FtanElement.CONTENT_KEY -> FtanArray(
                      FtanElement())))))),
            FtanString("after"))))) <--
      "<'outer\\'tag' style='outer'|start<inner|<second attr=null|<content=['third',<content=[<>]>]>>after>>"
  }

  they should "output invalid names as an attribute" in {
    "<name=3.0>" <-- FtanElement(FtanElement.NAME_KEY -> FtanNumber(3)) <-- ("<name=3.0>", "<name=3>")
    "<name=null>" <--> FtanElement(FtanElement.NAME_KEY -> FtanNull)
    "<name=[\"bla\"]>" <--> FtanElement(FtanElement.NAME_KEY -> FtanArray(FtanString("bla")))
    "<name=<bla>>" <--> FtanElement(FtanElement.NAME_KEY -> FtanElement(FtanElement.NAME_KEY -> FtanString("bla")))
    "<name=1.2|myContent<innerName>afterContent>" <--> FtanElement(FtanElement.NAME_KEY -> FtanNumber(1.2), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanString("afterContent")))
  }

  they should "output invalid content as an attribute" in {
    "<content=\"bla\">" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanString("bla"))
    "<content=[\"bla\",0.0]>" <--> FtanElement(FtanElement.CONTENT_KEY -> FtanArray(FtanString("bla"), FtanNumber(0)))
    "<a1:2 content=[\"myContent\",<innerName>,1.0,\"afterContent\"]>" <--> FtanElement(FtanElement.NAME_KEY -> FtanString("a1:2"), FtanElement.CONTENT_KEY -> FtanArray(FtanString("myContent"), FtanElement(FtanElement.NAME_KEY -> FtanString("innerName")), FtanNumber(1.0), FtanString("afterContent")))
  }

  they should "preserve the attribute order" in {
    "<a=1.0 b=2.0>" <--> FtanElement(FtanString("a") -> FtanNumber(1.0), FtanString("b") -> FtanNumber(2.0))
    "<a=2.0 b=1.0>" <--> FtanElement(FtanString("a") -> FtanNumber(2.0), FtanString("b") -> FtanNumber(1.0))
    "<b=1.0 a=2.0>" <--> FtanElement(FtanString("b") -> FtanNumber(1.0), FtanString("a") -> FtanNumber(2.0))
    "<b=2.0 a=1.0>" <--> FtanElement(FtanString("b") -> FtanNumber(2.0), FtanString("a") -> FtanNumber(1.0))
  }

  they should "have a default value when used directly" in {
    FtanElement should_equal FtanElement()
    FtanElement() should_equal FtanElement(new LinkedHashMap[FtanString, FtanValue])
  }

  they should "be rejected, if wrong" in {
    "<3>" invalid; //Invalid name
    //TODO
  }
}