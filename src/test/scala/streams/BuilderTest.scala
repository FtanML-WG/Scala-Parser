package streams

import _root_.util.ParserTest
import org.scalatest.FlatSpec
import ftanml.objects._
import java.io.StringWriter

/**
 * Unit tests for Builder constructing values from events
 */

class BuilderTest extends ParserTest with FlatSpec {

  val parse = (TestParser.parse _)

  def serialize(value:FtanValue) = {
    val sw = new StringWriter()
    value.send(new ftanml.streams.Serializer(sw, false))
    sw.toString
  }

  def roundTrips(input:String) : Boolean = {
    val afterParsing = parse(input)
    val builder = new ftanml.streams.Builder()
    afterParsing.send(builder)
    println(builder.value.toFtanML)
    afterParsing == builder.value
  }

  "Nulls" should "effectively round-trip" in {
    roundTrips("null")
    roundTrips("[null, 3]")
    roundTrips("<a=3 b=null>")
  }

  "Strings" should "effectively round-trip" in {
    roundTrips("\"abcd\"")
    roundTrips("\"abcd\\n\"")
    roundTrips("\"\\x2a6d6;\"")
  }

  "Numbers" should "effectively round-trip" in {
    roundTrips("1.23")
    roundTrips("-4")
    roundTrips("[-4,-5,-6]")
    roundTrips("<x=12 y=13>")
  }

  "Booleans" should "effectively round-trip" in {
    roundTrips("true")
    roundTrips("[false]")
    roundTrips("<|<|<a=false>>>")
  }

  "Arrays" should "effectively round-trip" in {
    roundTrips("[]")
    roundTrips("[true, false]")
    roundTrips("[1,2,3]")
    roundTrips("[[], [true]]")
    roundTrips("[<a>]")
    roundTrips("<a=[[1],[2],[3]]>")
  }

  "Elements" should "effectively round-trip" in {
    roundTrips("<>")
    roundTrips("<a>")
    roundTrips("<'1'>")
    roundTrips("<a=true>")
    roundTrips("<|abc>")
    roundTrips("<|abc<i|d>efg>")
    roundTrips("<|<><a><b|d>>")
    roundTrips("<a='1' b='2'>")
    roundTrips("<a='x' b='y'>")
  }

}