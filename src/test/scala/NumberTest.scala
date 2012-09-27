import ftanml.objects.FtanNumber
import ftanml.objects.FtanNull
import org.scalatest.FlatSpec

class NumberTest extends ParserTest with FlatSpec {

  "Numbers" should "be compared correctly (equals, hashCode)" in {

    FtanNumber(0) should_equal FtanNumber(0)
    FtanNumber(123) should_equal FtanNumber(123)
    FtanNumber(123.456) should_equal FtanNumber(123.456)
    FtanNumber(-123) should_equal FtanNumber(-123)
    FtanNumber(0) should_not_equal FtanNumber(0.1)
    FtanNumber(123) should_not_equal FtanNumber(123.456)
    FtanNumber(123.456) should_not_equal FtanNumber(123.654)
    FtanNumber(-123) should_not_equal FtanNumber(123)
    FtanNumber(-123.456) should_not_equal FtanNumber(123.456)
  }

  they should "be parsed correctly" in {
    //TODO allow parsing ".0" as "0.0"?
    //TODO allow parsing "0." as "0.0"?
    "0.0" <-- FtanNumber(0) <-- (
      "0", "0.0", "0.00000",
      "0e0", "0e+0", "0e-0", "0e100", "0e+100", "0e-100",
      "0E0", "0E+0", "0E-0", "0E100", "0E+100", "0E-100")
    "123.0" <-- FtanNumber(123) <-- (
      "123", "123.0", "123.00000",
      "123e0", "123e+0", "123e-0",
      "123E0", "123E+0", "123E-0")
    "123.456" <-- FtanNumber(123.456) <-- (
      "123.456", "123.4560000",
      "123.456e0", "123.456e+0", "123.456e-0",
      "123.456E0", "123.456E+0", "123.456E-0",
      "1.23456e2", "1.23456e+2", "12345.6e-2",
      "1.23456E2", "1.23456E+2", "12345.6E-2")
    "-123.0" <-- FtanNumber(-123) <-- (
      "-123", "-123.0", "-123.000",
      "-123e0", "-123e+0", "-123e-0",
      "-123E0", "-123E+0", "-123E-0")
    "-123.456" <-- FtanNumber(-123.456) <-- (
      "-123.456", "-123.4560000",
      "-123.456e0", "-123.456e+0", "-123.456e-0",
      "-123.456E0", "-123.456E+0", "-123.456E-0")
  }

  they should "have a default value when used directly" in {
    FtanNumber should_equal FtanNumber(0)
  }

  they should "be rejected, if wrong" in {
    //TODO
  }

}