package test.types

import org.scalatest.FlatSpec

import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

class FixedValueTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "Boolean Values" should "be instances of Fixed ValueType" in {
    assert(FtanBoolean(true).isInstance(new FixedValueType(FtanTrue)), "1")
    assert(FtanBoolean(false).isInstance(new FixedValueType(FtanFalse)), "2")
    assert(FtanFalse.isInstance(new FixedValueType(FtanBoolean(false))), "3")
    assert(FtanTrue.isInstance(new FixedValueType(FtanBoolean(true))), "4")
    assert(!FtanBoolean(true).isInstance(new FixedValueType(FtanFalse)), "5")
    assert(!FtanBoolean(false).isInstance(new FixedValueType(FtanTrue)), "6")
    assert(!FtanFalse.isInstance(new FixedValueType(FtanBoolean(true))), "7")
    assert(!FtanTrue.isInstance(new FixedValueType(FtanBoolean(false))), "8")
    assert(!FtanTrue.isInstance(new FixedValueType(FtanString("true"))), "9")
    assert(!new FixedValueType(FtanBoolean(false)).matches(FtanTrue), "10")
  }

  "Number Values" should "be instances of Fixed ValueType" in {
    assert(FtanNumber(93.7).isInstance(new FixedValueType(FtanNumber(93.7))))
    assert(!FtanNumber(93.7).isInstance(new FixedValueType(FtanNumber(93.8))))
    assert(!FtanNumber(93.7).isInstance(new FixedValueType(FtanArray(FtanNumber(93.7)))))
    assert(new FixedValueType(FtanNumber(0)).matches(FtanNumber(0)))
    assert(!new FixedValueType(FtanNumber(0)).matches(FtanFalse))
  }

  "Strings" should "be instances of Fixed ValueType" in {
    assert(FtanString("abcd").isInstance(new FixedValueType(FtanString("abcd"))))
    assert(!FtanString("abcd").isInstance(new FixedValueType(FtanString("pqrs"))))
    assert(new FixedValueType(FtanString("abcd")).matches(parse("\"abcd\"")))
  }

  "Arrays" should "be instances of Fixed ArrayType" in {
    assert(FtanArray(FtanTrue, FtanFalse).isInstance(new FixedValueType(parse("[true, false]"))))
    assert(FtanArray(FtanString("a"), FtanString("b")).isInstance(new FixedValueType(parse("['a', 'b']"))))
    assert(FtanArray().isInstance(new FixedValueType(parse("[]"))))
    assert(!FtanArray(FtanTrue, FtanFalse).isInstance(new FixedValueType(parse("[true, false, true]"))))
    assert(!FtanArray(FtanString("a"), FtanString("b")).isInstance(new FixedValueType(parse("['a']"))))
    assert(!FtanArray().isInstance(new FixedValueType(parse("[null]"))))
  }

  "Elements" should  "be instances of ElementType" in {
    // TODO add whitespace tests when the rules are clear
    // TODO reinstate ignored tests when we agree that <a>==<a|> is true.
    assert(parse("<>").isInstance(new FixedValueType(parse("< >"))), "1")
    assert(parse("<b>").isInstance(new FixedValueType(parse("< 'b' >"))), "2")
    //TODO assert(parse("<b|>").isInstance(new FixedValueType(parse("< 'b' >"))), "3")
    //TODO assert(parse("<b foo=[1,2,3]|>").isInstance(new FixedValueType(parse("< 'b' foo=[1, 2, 3]>"))), "4")
    assert(parse("<a=1 b=2>").isInstance(new FixedValueType(parse("<b=2 a=1>"))), "5")
    assert(!parse("<>").isInstance(new FixedValueType(parse("<|z>"))), "6")
    assert(!parse("<b>").isInstance(new FixedValueType(parse("< 'b' |c>"))), "7")
    //TODO assert(!parse("<b|>").isInstance(new FixedValueType(parse("< 'b' >"))), "8")
    //TODO assert(!parse("<b foo=[1,2,3]|>").isInstance(new FixedValueType(parse("< 'b' foo=[1, 3, 2]>"))), "9")
    assert(!parse("<a=1 b=2>").isInstance(new FixedValueType(parse("<b=1 a=2>"))), "10")
  }


}