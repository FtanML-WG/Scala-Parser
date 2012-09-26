package test.types

import org.scalatest.FlatSpec

import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SizeTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "Size types" should "match Strings, Arrays and Elements" in {
  	assert(FtanString("test").isInstance(new SizeType(FtanNumber(4))), "1")
  	assert(FtanString("test").isInstance(new MinSizeType(FtanNumber(4))), "2")
  	assert(!FtanString("test").isInstance(new MinSizeType(FtanNumber(5))), "3")
  	assert(!FtanString("test").isInstance(new MaxSizeType(FtanNumber(3))), "4")
  	
  	assert(new SizeType(FtanNumber(4)).matches(parse("\\\"\\\\\\u0040\\x41;").asInstanceOf[FtanString]), "")
  }


}