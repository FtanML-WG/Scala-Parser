package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

/**
 * Unit tests for enumeration types
 */

class EnumerationTest extends FlatSpec with TypeTest {
  
  val parse = TestParser.parse _

  "Boolean Values" should "be instances of Enumeration Type" in {
    FtanBoolean(true) ==> new EnumerationType(Seq(FtanTrue, FtanFalse))
    FtanBoolean(false) ==> new EnumerationType(Seq(FtanFalse, new FtanString("")))
    FtanFalse !=> new EnumerationType(Seq())
  }

  "Number Values" should "be instances of Enumeration Type" in {
    FtanNumber(93.7) ==> new EnumerationType(Seq(FtanNumber(93.7)))
    FtanNumber(93.7) ==> new EnumerationType(Seq(FtanString("z"), FtanNumber(93.7)))
    FtanNumber(93.7) !=> new EnumerationType(Seq(FtanFalse,  FtanList(FtanNumber(93.7))))
  }

  "Strings" should "be instances of Enumeration Type" in {
    FtanString("abcd") ==> new EnumerationType(Seq(FtanString("abcd"), FtanString("pqrs")))
    FtanString("abcd") !=> new EnumerationType(Seq(FtanString(""), FtanString("pqrs")))
    "\"abcd\"" ==> new EnumerationType(Seq(FtanString("abcd"), FtanString("abcd"))) 
  }

  "Arrays" should "be instances of Enumeration Type" in {
    FtanList(FtanTrue, FtanFalse) ==> new EnumerationType(Seq(FtanList(), FtanList(parse("true"), parse("false"))))
  }

  "Elements" should  "be instances of ElementType" in {
    "<a b=1 c=2>" ==> new EnumerationType(
      Seq(FtanList(), parse("<a c=2 b=1>")))
  }


}