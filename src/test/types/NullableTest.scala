package test.types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types.NullableType

class NullableTest extends FlatSpec {

	"Null" should "be matched correctly by NullableType" in {
		assert(FtanNull.isInstance(new NullableType(FtanTrue)), "1")
		assert(!FtanNull.isInstance(new NullableType(FtanFalse)), "2")
		assert(new NullableType(FtanTrue).matches(FtanNull), "3")
		assert(!new NullableType(FtanFalse).matches(FtanNull), "4")
	}
	
	"Other values" should "be unaffected by NullableType" in {
		assert(FtanFalse.isInstance(new NullableType(FtanTrue)), "1")
		assert(FtanFalse.isInstance(new NullableType(FtanFalse)), "2")
		assert(FtanNumber(0).isInstance(new NullableType(FtanFalse)), "3")
		assert(FtanArray(FtanString("")).isInstance(new NullableType(FtanFalse)), "4")
		assert(new NullableType(FtanTrue).matches(FtanNumber(3)), "5")
		assert(new NullableType(FtanFalse).matches(FtanElement(FtanString("bla") -> FtanString("1.0"))), "6")
	}
}