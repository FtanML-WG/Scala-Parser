package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class PrimitiveTypeTest extends FlatSpec with TypeTest {

  "Booleans" should "be instances of BooleanType" in {
    FtanBoolean(true) ==> BooleanType
    FtanBoolean(false) ==> BooleanType
    FtanBoolean(false) !=> NumberType
    FtanNumber(3) !=> BooleanType
    FtanTrue ==> BooleanType
    FtanFalse ==> BooleanType
    "true" ==> BooleanType
    "false" ==> BooleanType
    "[]" !=> BooleanType
  }

  "Numbers" should "be instances of NumberType" in {
    FtanNumber(93.7) ==> NumberType
    FtanNumber(82.6) !=> StringType
    "1.23" ==> NumberType
    "true" !=> NumberType
  }

  "Strings" should "be instances of StringType" in {
    FtanString("abcd") ==> StringType
    FtanString("123") !=> NumberType
    "\"foo\"" ==> StringType
    "123" !=> StringType
  }

  "Arrays" should "be instances of ArrayType" in {
    FtanArray(FtanTrue, FtanFalse) ==> ArrayType
    FtanArray(FtanString("a"), FtanString("b")) ==> ArrayType
    FtanArray(FtanArray(), FtanString("a"), FtanString("b")) ==> ArrayType
    FtanTrue !=> ArrayType
    FtanArray() !=> BooleanType
    "[]" ==> ArrayType
    "[[],[]]" ==> ArrayType
    "[1,2,3]" ==> ArrayType
  }

  "Elements" should  "be instances of ElementType" in {
    "<>" ==> ElementType
    "<b>" ==> ElementType
    "<b|>" ==> ElementType
    "<b foo=[1,2,3]|>" ==> ElementType
  }

  "Everything" should "match AnyType" in  {
    "null" ==> AnyType
    "3.14" ==> AnyType
    "\"London\"" ==> AnyType
    "[1,2,3]"  ==> AnyType
    "<e>" ==> AnyType
  }

  "Nothing" should "match NothingType" in  {
    "null" !=> NothingType
    "3.14" !=> NothingType
    "\"London\"" !=> NothingType
    "[1,2,3]"  !=> NothingType
    "<e>" !=> NothingType
  }


}