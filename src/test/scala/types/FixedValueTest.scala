package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class FixedValueTest extends FlatSpec with TypeTest {
  
  val parse = TestParser.parse _

  "Boolean Values" should "be instances of Fixed ValueType" in {
    FtanBoolean(true) ==> new FixedValueType(FtanTrue)
    FtanBoolean(false) ==> new FixedValueType(FtanFalse)
    FtanFalse ==> new FixedValueType(FtanBoolean(false))
    FtanTrue ==> new FixedValueType(FtanBoolean(true))
    FtanBoolean(true) !=> new FixedValueType(FtanFalse)
    FtanBoolean(false) !=> new FixedValueType(FtanTrue)
    FtanFalse !=> new FixedValueType(FtanBoolean(true))
    FtanTrue !=> new FixedValueType(FtanBoolean(false))
    FtanTrue !=> new FixedValueType(FtanString("true"))
  }

  "Number Values" should "be instances of Fixed ValueType" in {
    FtanNumber(93.7) ==> new FixedValueType(FtanNumber(93.7))
    FtanNumber(93.7) !=> new FixedValueType(FtanNumber(93.8))
    FtanNumber(93.7) !=> new FixedValueType(FtanArray(FtanNumber(93.7)))
    FtanNumber(0) ==> new FixedValueType(FtanNumber(0))
    FtanFalse !=> new FixedValueType(FtanNumber(0))
  }

  "Strings" should "be instances of Fixed ValueType" in {
    FtanString("abcd") ==> new FixedValueType(FtanString("abcd"))
    FtanString("abcd") !=> new FixedValueType(FtanString("pqrs"))
    "\"abcd\"" ==> new FixedValueType(FtanString("abcd"))
    "\"\\x41;\"" ==> new FixedValueType(FtanString("A"))
    "\"\\x041;\"" ==> new FixedValueType(FtanString("A"))
    "\"\\x20ac;\"" ==> new FixedValueType(FtanString("\u20AC"))
    "\"\\xa;\"" ==> new FixedValueType(FtanString("\n"))
    "\"\\x2A6d6;\"" ==> new FixedValueType(FtanString("\uD869\uDED6"))
    "\"\\[?abcd?]\"" ==> new FixedValueType(FtanString("abcd"))
    "\"\\[**]\"" ==> new FixedValueType(FtanString(""))
    "\"\\[*<a>*]\"" ==> new FixedValueType(FtanString("<a>"))
    "\"\\[***]\"" ==> new FixedValueType(FtanString("*"))
    "\"\\[*]*]\"" ==> new FixedValueType(FtanString("]"))
    "\"\\[§\\<\\>§]\"" ==> new FixedValueType(FtanString("\\<\\>"))
  }

  "Arrays" should "be instances of Fixed ArrayType" in {
    FtanArray(FtanTrue, FtanFalse) ==> new FixedValueType(parse("[true,false]"))
    FtanArray(FtanString("a"), FtanString("b")) ==> new FixedValueType(parse("['a','b']"))
    FtanArray() ==> new FixedValueType(parse("[]"))
    FtanArray(FtanTrue, FtanFalse) !=> new FixedValueType(parse("[true,false,true]"))
    FtanArray(FtanString("a"), FtanString("b")) !=> new FixedValueType(parse("['a']"))
    FtanArray() !=> new FixedValueType(parse("[null]"))
  }

  "Elements" should  "be instances of ElementType" in {
    // TODO add whitespace tests when the rules are clear
    // TODO reinstate ignored tests when we agree that <a>==<a|> is true.
    "<>" ==> new FixedValueType(parse("< >"))
    "<b>" ==> new FixedValueType(parse("< 'b' >"))
    //TODO assert(parse("<b|>").isInstance(new FixedValueType(parse("< 'b' >"))), "3")
    //TODO assert(parse("<b foo=[1,2,3]|>").isInstance(new FixedValueType(parse("< 'b' foo=[1, 2, 3]>"))), "4")
    "<a=1 b=2>" ==> new FixedValueType(parse("<b=2 a=1>"))
    "<>" !=> new FixedValueType(parse("<|z>"))
    "<b>" !=> new FixedValueType(parse("< 'b' |c>"))
    //TODO assert(!parse("<b|>").isInstance(new FixedValueType(parse("< 'b' >"))), "8")
    //TODO assert(!parse("<b foo=[1,2,3]|>").isInstance(new FixedValueType(parse("< 'b' foo=[1, 3, 2]>"))), "9")
    "<a=1 b=2>" !=> new FixedValueType(parse("<b=1 a=2>"))
  }


}