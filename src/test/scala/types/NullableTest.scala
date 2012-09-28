package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types.NullableType
import util.TypeTest

class NullableTest extends FlatSpec with TypeTest {

	"Null" should "be matched correctly by NullableType" in {
		FtanNull ==> new NullableType(FtanTrue)
		FtanNull !=> new NullableType(FtanFalse)
	}
	
	"Other values" should "be unaffected by NullableType" in {
		FtanFalse ==> new NullableType(FtanTrue)
		FtanFalse ==> new NullableType(FtanFalse)
		FtanNumber(0) ==> new NullableType(FtanFalse)
		FtanArray(FtanString("")) ==> new NullableType(FtanFalse)
		FtanNumber(3) ==> new NullableType(FtanTrue)
		FtanElement(FtanString("bla") -> FtanString("1.0")) ==> new NullableType(FtanFalse)
	}
}