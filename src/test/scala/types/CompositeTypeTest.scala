package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

/**
 * Unit tests for AnyOf and AllOf types
 */

class CompositeTypeTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  var threeNumbers = new EnumerationType(
    Seq(FtanNumber(1), FtanNumber(2), FtanNumber(3))
  )

  var fixedNumber = new FixedValueType(FtanNumber(2))

  var anyString = StringType;

  var anyNumber = NumberType;

  var anyType = new AnyOfType(Seq(threeNumbers, anyString))

  var allType = new AllOfType(Seq(threeNumbers, fixedNumber, anyNumber))

  "Values" should "be instances of combined Types" in {
    assert(!FtanBoolean(true).isInstance(anyType), "1")
    assert(!FtanTrue.isInstance(allType), "2")
    assert(FtanNumber(2).isInstance(anyType), "3");
    assert(FtanNumber(2).isInstance(allType), "4");
    assert(!FtanNumber(3).isInstance(allType), "5");
    assert(FtanString("").isInstance(anyType), "6");
    assert(anyType.matches(FtanString("")))
  }


}