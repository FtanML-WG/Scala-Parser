package types

import org.scalatest.FlatSpec

import ftanml.objects.FtanBoolean
import ftanml.objects.FtanNumber
import ftanml.objects.FtanString
import ftanml.objects.FtanTrue
import ftanml.types.AllOfType
import ftanml.types.AnyOfType
import ftanml.types.EnumerationType
import ftanml.types.FixedValueType
import ftanml.types.NumberType
import ftanml.types.StringType
import util.TypeTest

/**
 * Unit tests for AnyOf and AllOf types
 */

class CompositeTypeTest extends FlatSpec with TypeTest {

  var threeNumbers = new EnumerationType(
    Seq(FtanNumber(1), FtanNumber(2), FtanNumber(3))
  )

  var fixedNumber = new FixedValueType(FtanNumber(2))

  var anyString = StringType;

  var anyNumber = NumberType;

  var anyType = new AnyOfType(Seq(threeNumbers, anyString))

  var allType = new AllOfType(Seq(threeNumbers, fixedNumber, anyNumber))

  "Values" should "be instances of combined Types" in {
    FtanBoolean(true) !=> anyType
    FtanTrue !=> allType
    FtanNumber(2) ==> anyType
    FtanNumber(2) ==> allType
    FtanNumber(3) !=> allType
    FtanString("") ==> anyType
  }


}