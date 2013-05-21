package streams

import _root_.util.ParserTest
import org.scalatest.FlatSpec
import ftanml.objects._
import java.io.StringWriter
import ftanml.streams.JSONSerializerFactory

/**
 * Unit tests for JSON Serialization of various values
 */

class JSONSerializerTest extends ParserTest with FlatSpec {

  val parse = (TestParser.parse _)

  def serialize(value:FtanValue) = {
    val sw = new StringWriter()
    value.send(JSONSerializerFactory.make(sw, false))
    sw.toString
  }

  "Nulls" should "effectively round-trip" in {
    serialize(parse("null")) should_equal "null"
    serialize(parse("[null]")) should_equal "[null]"
  }

  "Strings" should "effectively round-trip" in {
    serialize(parse("\"abcd\"")) should_equal "\"abcd\""
    serialize(parse("\"abcd\\n\"")) should_equal "\"abcd\\n\""
    serialize(parse("\"\\x2A6d6;\"")) should_equal "\"\\x2a6d6;\""
  }

  "Numbers" should "effectively round-trip" in {
    serialize(parse("1.23")) should_equal "1.23"
    serialize(parse("-4")) should_equal "-4"
  }

  "Booleans" should "effectively round-trip" in {
    serialize(parse("true")) should_equal "true"
    serialize(parse("false")) should_equal "false"
  }

  "Arrays" should "effectively round-trip" in {
    serialize(parse("[]")) should_equal "[]"
    serialize(parse("[true,false]")) should_equal "[true,false]"
    serialize(parse("[true,false]")) should_equal "[true,false]"
    serialize(parse("[1,2,3]")) should_equal "[1,2,3]"
    serialize(parse("[[],[true]]")) should_equal "[[],[true]]"
  }

  "Elements" should "be converted" in {
    serialize(parse("<>")) should_equal "{}"
    serialize(parse("<a>")) should_equal "{\"$name\":\"a\"}"
    serialize(parse("<`1`>")) should_equal "{\"$name\":\"1\"}"
    serialize(parse("<a=true>")) should_equal "{\"a\":true}"
    serialize(parse("<|abc|>")) should_equal "{\"$content\":[\"abc\"]}"
    serialize(parse("<|a<b>c|>")) should_equal "{\"$content\":[\"a\",{\"$name\":\"b\"},\"c\"]}"
    serialize(parse("<|abc<i|d|>efg|>")) should_equal "{\"$content\":[\"abc\",{\"$name\":\"i\",\"$content\":[\"d\"]},\"efg\"]}"
    serialize(parse("<|<><a><b|d|>|>")) should_equal "{\"$content\":[{},{\"$name\":\"a\"},{\"$name\":\"b\",\"$content\":[\"d\"]}]}"
    serialize(parse("<a=\"1\" b=\"2\">")) should_equal "{\"a\":\"1\",\"b\":\"2\"}"
    serialize(parse("<a=\"x\" b=\"y\">")) should_equal "{\"a\":\"x\",\"b\":\"y\"}"
  }

}