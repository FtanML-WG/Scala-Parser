package util

import org.scalatest.Suite
import ftanml.types.FtanType
import ftanml.objects.FtanValue

/**
 * When you inherit your test class from TypeTest, you can use matchers like
 * 
 * "[3,4]" ==> new SizeType(2) //The parser must return a FtanValue that matches SizeType(2) when parsing "[3,4]"
 * new SizeType(2) <== "[3,4]"  //The parser must return a FtanValue that matches SizeType(2) when parsing "[3,4]"
 * 
 * or using FtanValues directly
 * 
 * FtanArray(FtanNumber(3),FtanNumber(4)) ==> new SizeType(2) //FtanArray(FtanNumber(3),FtanNumber(4)).isInstance(new SizeType(2)) must return true
 * new SizeType(2) <== FtanArray(FtanNumber(3),FtanNumber(4)) //Same as above
 * 
 * You can also use !=> and <=! to express that a value should not have the given type.
 * 
 * It always automatically checks both value.isInstance(type) and type.matches(value)
 * 
 */
trait TypeTest extends ParserTest {
  self: Suite =>
    
  //Allow the isInstance and matches tests
  implicit def str2TypeTest(str: String) = new {
    def ==>(t: FtanType) {
      if (DEBUG) Console.println("parsing: " + str)
      val parsed = TestParser.parsing(str) 
      parsed.isInstance(t) should equal(true)
      t.matches(parsed) should equal(true)
    }
    def !=>(t: FtanType) {
      if (DEBUG) Console.println("parsing: " + str)
      val parsed = TestParser.parsing(str)
      parsed.isInstance(t) should equal(false)
      t.matches(parsed) should equal(false)
    }
  }
  implicit def val2TypeTest(value: FtanValue) = new {
    def ==>(t: FtanType) {
      if (DEBUG) Console.err.println("testing: " + value + " is-instance-of " + t)
      value.isInstance(t) should equal(true)
    }
    def !=>(t: FtanType) {
      if (DEBUG) Console.err.println("testing: " + value+ " is-not-instance-of " + t)
      value.isInstance(t) should equal(false)
    }
  }
  implicit def type2TypeTest(t: FtanType) = new {
    def <==(input: String) { input ==> t }
    def <==(input: FtanValue) { input ==> t }
    def <=!(input: String) { input !=> t }
    def <=!(input: FtanValue) { input !=> t }
  }

  val DEBUG = true
}