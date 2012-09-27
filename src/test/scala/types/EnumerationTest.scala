package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

/**
 * Unit tests for enumeration types
 */

class EnumerationTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "Boolean Values" should "be instances of Enumeration Type" in {
    assert(FtanBoolean(true).isInstance(new EnumerationType(Seq(FtanTrue, FtanFalse))), "1")
    assert(FtanBoolean(false).isInstance(new EnumerationType(Seq(FtanFalse, new FtanString("")))), "2")
    assert(!FtanFalse.isInstance(new EnumerationType(Seq())), "3")
  }

  "Number Values" should "be instances of Enumeration Type" in {
    assert(FtanNumber(93.7).isInstance(new EnumerationType(Seq(FtanNumber(93.7)))))
    assert(FtanNumber(93.7).isInstance(new EnumerationType(Seq(FtanString("z"), FtanNumber(93.7)))))
    assert(!FtanNumber(93.7).isInstance(new EnumerationType(Seq(FtanFalse,  FtanArray(FtanNumber(93.7))))))
  }

  "Strings" should "be instances of Enumeration Type" in {
    assert(FtanString("abcd").isInstance(new EnumerationType(Seq(FtanString("abcd"), FtanString("pqrs")))), "1");
    assert(!FtanString("abcd").isInstance(new EnumerationType(Seq(FtanString(""), FtanString("pqrs")))), "2");
    assert(new EnumerationType(Seq(FtanString("abcd"), FtanString("abcd"))).matches(parse("\"abcd\"")), "3");
  }

  "Arrays" should "be instances of Enumeration Type" in {
    assert(FtanArray(FtanTrue, FtanFalse).isInstance(new EnumerationType(Seq(FtanArray(), FtanArray(parse("true"), parse("false"))))))
  }

  "Elements" should  "be instances of ElementType" in {
    assert(parse("<a b=1 c=2>").isInstance(new EnumerationType(
      Seq(FtanArray(), parse("<a c=2 b=1>")))))
  }


}