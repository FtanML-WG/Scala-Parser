package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

/**
 * Unit tests for TypeFactory constructing types from elements
 */

class TypeFactoryTest extends FlatSpec with TypeTest {

  // TODO: convert to use the new syntax with no vertical bar when available

  def theType (s: String) = TypeFactory.makeType(parsel(s))

  val parsel = (TestParser.parse _) andThen { _.asInstanceOf[FtanElement] }

  val booleanType = TypeFactory.makeType(parsel("<boolean>"))

  val threeNumbers = TypeFactory.makeType(parsel("<enum=[1,2,3]>"))

  val fixedNumber = TypeFactory.makeType(parsel("<fixed=2>"))

  val anyString = TypeFactory.makeType(parsel("<string>"));

  val anyNumber = TypeFactory.makeType(parsel("<number>"))

  val unionType = TypeFactory.makeType(parsel("<anyOf|<enum=[1,2,3]><fixed=2><string>>"))

  val twoType = TypeFactory.makeType(parsel("<number enum=[1,2,3] fixed=2>"))

  val minMaxType = TypeFactory.makeType(parsel("<min=5 max=10>"))

  val minMaxExclusive = TypeFactory.makeType(parsel("<minExclusive=5 maxExclusive=10>"))

  val minMaxWithExclusions = TypeFactory.makeType(parsel("<allOf|<min=-5 max=5><not|<fixed=0>>>"))

  val nullableBoolean = TypeFactory.makeType(parsel("<nullable|<boolean>>"))

  val nullableString = TypeFactory.makeType(parsel("<nullable|<string>>"))

  val regexType = TypeFactory.makeType(parsel("<regex='[0-9a-f]+'>"))

  val anyType = TypeFactory.makeType(parsel("<any>"))

  val nothingType = TypeFactory.makeType(parsel("<nothing>"))

  val notStringType = TypeFactory.makeType(parsel("<not|<string>>"))

  "Booleans" should "be instances of factory-made Types" in {
    FtanBoolean(true) ==> booleanType
    FtanTrue ==> anyType
    FtanFalse ==> notStringType
    FtanTrue ==> nullableBoolean
    FtanTrue !=> anyString
    FtanTrue !=> nothingType
    FtanNull ==> nullableBoolean
  }

  "Strings" should "be instances of factory-made Types" in {
    FtanString("") ==> anyType
    FtanString("abc") ==> anyString
    FtanString("") !=> nullableBoolean
    FtanString("") ==> nullableString
    FtanString("03f") ==> regexType
    FtanString("03A") !=> regexType
    FtanString("") !=> nothingType
    FtanString("") ==> theType("<string size=0>")
    FtanString("abc") ==> theType("<string size=3>")
    FtanString("abc") !=> theType("<string size=2>")
    TestParser.parse("\"\\x10000;\"") ==> theType("<string size=1>")
  }

  "Enumerations" should "be instances of enumeration types" in  {
    FtanNumber(3) ==> theType("<enum=[1,2,3,4]>")
    FtanNumber(5) !=> theType("<enum=[1,2,3,4]>")
    FtanNumber(5) !=> theType("<enum=['a','b','c','d']>")
    FtanString("b") ==> theType("<enum=['a','b','c','d']>")
  }

  "Numbers" should "be instances of factory-made Types" in {
    FtanNumber(2) ==> anyType
    FtanNumber(2) ==> anyNumber
    FtanNumber(2) ==> fixedNumber
    FtanNumber(2) ==> twoType
    FtanNumber(3) !=> twoType
  }

  "MoreNumbers"  should "be instances of factory-made Types" in {
    FtanNumber(5) ==> minMaxType
    FtanNumber(10) ==> minMaxType
    FtanNumber(11) !=> minMaxType
    FtanNumber(4) !=> minMaxType
    FtanNumber(5) !=> minMaxExclusive
    FtanNumber(10) !=> minMaxExclusive
    FtanNumber(8) ==> minMaxExclusive
    FtanNumber(0) !=> minMaxWithExclusions
  }

  "Null" should "be instances of factory-made Types" in {
    FtanNull ==> nullableBoolean
    FtanNull ==> nullableString
    FtanNull ==> anyType
    FtanNull !=> anyString
  }

  "Arrays" should "be instances of factory-made Types" in {
    TestParser.parse("[1,2,3]") ==> theType("<array>")
    TestParser.parse("[1,2,3]") ==> theType("<nullable|<array>>")
    TestParser.parse("[1,2,3]") ==> theType("<array size=3 itemType=<number>>")
    TestParser.parse("[1,2,3]") ==> theType("<array size=3 itemType=<number min=1 max=3>>")
    TestParser.parse("[1,2,3]") !=> theType("<array size=4>")
    TestParser.parse("[1,2,3]") !=> theType("<array itemType=<boolean>>")
    FtanNull !=> theType("<array>")
    FtanNull ==> theType("<nullable|<array>>")
  }

  "EmptyElements" should "be instances of element types" in {
    TestParser.parse("<e>") ==> theType("<element>")
    TestParser.parse("<e>") ==> theType("<element|<e>>")
    TestParser.parse("<e a=3>") ==> theType("<element|<e a=<number max=3>>>")
    TestParser.parse("<e>") ==> theType("<element|<e a=<nullable|<number max=3>>>>")
    TestParser.parse("<e a=[]>") !=> theType("<element|<e a=<number>>>")
    TestParser.parse("<e b=5>") ==> theType("<element|<e a=<nullable|<number max=3>>>>")
    TestParser.parse("<e a=3>") ==> theType("<element|<a=<number max=3>>>")
  }

  "NamedElements" should "be instances of element types" in {
    TestParser.parse("<e>") ==> theType("<element name=<fixed='e'>>")
    TestParser.parse("<e>") ==> theType("<element name=<regex='[a-z]'>>")
    TestParser.parse("<a>") ==> theType("<element name=<enum=['a','b','c','d']>>")
    TestParser.parse("<h>") !=> theType("<element name=<fixed='e'>>")
    TestParser.parse("<H>") !=> theType("<element name=<regex='[a-z]'>>")
    TestParser.parse("<h>") !=> theType("<element name=<enum=['e','f','g']>>")
  }

  "RestrictedElements" should "be instances of attName types" in  {
    TestParser.parse("<a=3 b=4>") ==> theType("<element attName=<enum=['a','b']>>")
    TestParser.parse("<a=3 f=4>") !=> theType("<element attName=<enum=['a','b']>>")
    TestParser.parse("<eeee a=3 f=4 |content>") ==> theType("<element attName=<regex='[a-g]'>>")
    TestParser.parse("<eeee a=3 h=4 |content>") !=> theType("<element attName=<regex='[a-g]'>>")
  }

  "ArrayGrammar" should "match array instances" in {
    TestParser.parse("[1, 2, 3]") ==> theType("<array |<number><number><number>>")
    TestParser.parse("[1, 2, 3]") ==> theType("<array|<many|<number>>>")
    TestParser.parse("[]") ==> theType("<array|<many|<number>>>")
    TestParser.parse("[]") ==> theType("<array|<optional|<number>>>")
    TestParser.parse("[1]") ==> theType("<array|<optional|<number>>>")
    TestParser.parse("[1,2]") !=> theType("<array|<optional|<number>>>")
    TestParser.parse("['a',1,2]") ==> theType("<array|<string><many|<number>>>")
    TestParser.parse("['a']") ==> theType("<array|<string><many|<number>>>")
    TestParser.parse("['a',1,2,'b']") !=> theType("<array|<string><many|<number>>>")
    TestParser.parse("[-2,-3,-4,2,3,4]") ==> theType("<array|<many|<maxExclusive=0>><many|<minExclusive=0>>>")
    TestParser.parse("[-2,-3,-4,2,3,4,-1]") !=> theType("<array|<many|<maxExclusive=0>><many|<minExclusive=0>>>")
    TestParser.parse("[-2,-3,-4,2,3,4,-1]") ==> theType("<array|<anyOf max=10|<maxExclusive=0><minExclusive=0>>>")
    TestParser.parse("[-2,-3,-4,2,3,4,-1,0]") !=> theType("<array|<anyOf max=10|<maxExclusive=0><minExclusive=0>>>")
    TestParser.parse("[-2,-3,-4,2,3,4,-1,8,-3,10,11]") !=> theType("<array|<anyOf max=10|<maxExclusive=0><minExclusive=0>>>")
  }

  "ElementGrammar" should "match element instances" in {
    TestParser.parse("<e|<f><g><h>>") ==> theType("<element|<e|<element|<f>><element|<g>><element|<h>>>>")
    TestParser.parse("<e|<f><g><i>>") !=> theType("<element|<e|<element|<f>><element|<g>><element|<h>>>>")
    TestParser.parse("<e|<f><g><i>>") ==> theType("<element|<e|<element|<f>><element|<g>><anyOf|<element|<h>><element|<i>>>>>")
    TestParser.parse("<e|<f><g><i><h>>") ==> theType("<element|<e|<element|<f>><element|<g>><anyOf max=2|<element|<h>><element|<i>>>>>")
    TestParser.parse("<e|<f><g><i><h><i>>") !=> theType("<element|<e|<element|<f>><element|<g>><anyOf max=2|<element|<h>><element|<i>>>>>")
  }

}