package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class SizeTest extends FlatSpec with TypeTest {

  "Size types" should "match Strings, Lists and Elements" in {
  	FtanString("test") ==> new SizeType(4)
  	FtanString("test") ==> new MinSizeType(4)
  	FtanString("test") !=> new MinSizeType(5)
  	FtanString("test") !=> new MaxSizeType(3)
  	
  	"\"\\\"\\\\\\x0040;\\x41;\"" ==> new SizeType(4)
  	
  	FtanList() ==> new SizeType(0)
  	FtanList(FtanString("test"), FtanString("test")) ==> new MinSizeType(1)
  	
  	"<att1=1 att2=2>" ==> new SizeType(2)
  	"<elem att1=1 att2=2 'content'>" ==> new MaxSizeType(2)
  	"<elem'content'>" !=> new MinSizeType(1)
  }

  it should "not match any other FtanValues" in {
  	FtanNumber(2) !=> new SizeType(2)
  	FtanBoolean(true) !=> new MaxSizeType(2)
  }
}