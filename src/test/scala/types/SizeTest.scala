package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class SizeTest extends FlatSpec with TypeTest {

  "Size types" should "match Strings, Arrays and Elements" in {
  	FtanString("test") ==> new SizeType(FtanNumber(4))
  	FtanString("test") ==> new MinSizeType(FtanNumber(4))
  	FtanString("test") !=> new MinSizeType(FtanNumber(5))
  	FtanString("test") !=> new MaxSizeType(FtanNumber(3))
  	
  	"\"\\\"\\\\\\u0040\\x41;\"" ==> new SizeType(FtanNumber(4))
  	
  	FtanArray() ==> new SizeType(FtanNumber(0))
  	FtanArray(FtanString("test"), FtanString("test")) ==> new MinSizeType(FtanNumber(1))
  	
  	"<att1=1 att2=2>" ==> new SizeType(FtanNumber(2))
  	"<elem att1=1 att2=2|content>" ==> new MaxSizeType(FtanNumber(2))
  	"<elem|content>" !=> new MinSizeType(FtanNumber(1))
  }

  it should "not match any other FtanValues" in {
  	FtanNumber(2) !=> new SizeType(FtanNumber(2))
  	FtanBoolean(true) !=> new MaxSizeType(FtanNumber(2))
  }
}