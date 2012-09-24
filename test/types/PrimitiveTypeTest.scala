package test.types

import org.scalatest.FlatSpec

import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

class PrimitiveTypeTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "Booleans" should "be instances of BooleanType" in {
    assert(FtanBoolean(true).isInstance(BooleanType))
    assert(FtanBoolean(false).isInstance(BooleanType))
    assert(!FtanBoolean(false).isInstance(NumberType))
    assert(!FtanNumber(3).isInstance(BooleanType))
    assert(FtanTrue.isInstance(BooleanType))
    assert(FtanFalse.isInstance(BooleanType))
    assert(parse("true").isInstance(BooleanType))
    assert(parse("false").isInstance(BooleanType))
    assert(BooleanType.matches(FtanFalse));
    assert(!BooleanType.matches(parse("[]")));
  }

  "Numbers" should "be instances of NumberType" in {
    assert(FtanNumber(93.7).isInstance(NumberType))
    assert(!FtanNumber(82.6).isInstance(StringType))
    assert(parse("1.23").isInstance(NumberType))
    assert(!NumberType.matches(parse("true")))
  }

  "Strings" should "be instances of StringType" in {
    assert(FtanString("abcd").isInstance(StringType))
    assert(!FtanString("123").isInstance(NumberType))
    assert(parse("\"foo\"").isInstance(StringType))
    assert(!StringType.matches(parse("123")))
  }

  "Arrays" should "be instances of ArrayType" in {
    assert(FtanArray(FtanTrue, FtanFalse).isInstance(ArrayType))
    assert(FtanArray(FtanString("a"), FtanString("b")).isInstance(ArrayType))
    assert(FtanArray(FtanArray(), FtanString("a"), FtanString("b")).isInstance(ArrayType))
    assert(!FtanTrue.isInstance(ArrayType))
    assert(!FtanArray().isInstance(BooleanType))
    assert(parse("[]").isInstance(ArrayType))
    assert(parse("[[],[]]").isInstance(ArrayType))
    assert(parse("[1,2,3]").isInstance(ArrayType))
    assert(ArrayType.matches(parse("[1,2,3]")))
  }

  "Elements" should  "be instances of ElementType" in {
    assert(parse("<>").isInstance(ElementType))
    assert(parse("<b>").isInstance(ElementType))
    assert(parse("<b|>").isInstance(ElementType))
    assert(parse("<b foo=[1,2,3]|>").isInstance(ElementType))
    assert(ElementType.matches(parse("<b>")))
  }


}