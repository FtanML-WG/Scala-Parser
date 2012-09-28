package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

/**
 * Unit tests for TypeFactory constructing types from elements
 */

class TypeFactoryTest extends FlatSpec with TypeTest {

  val parse = (TestParser.parse _) andThen { _.asInstanceOf[FtanElement] }

  val booleanType = TypeFactory.makeType(parse("<boolean>"))

  val threeNumbers = TypeFactory.makeType(parse("<enum=[1,2,3]>"))

  val fixedNumber = TypeFactory.makeType(parse("<fixed=2>"))

  val anyString = TypeFactory.makeType(parse("<string>"));

  val anyNumber = TypeFactory.makeType(parse("<number>"))

  val anyType = TypeFactory.makeType(parse("<anyOf=[<enum=[1,2,3]>, <fixed=2>, <string>]>"))

  val allType = TypeFactory.makeType(parse("<number enum=[1,2,3] fixed=2>"))

  val minMaxType = TypeFactory.makeType(parse("<min=5 max=10>"))

  val minMaxExclusive = TypeFactory.makeType(parse("<minExclusive=5 maxExclusive=10>"))

  val minMaxWithExclusions = TypeFactory.makeType(parse("<min=-5 max=5 not=<fixed=0>>"))
  
  val nameType = TypeFactory.makeType(parse("<name='test'>"))

  val nullableFalse = TypeFactory.makeType(parse("<nullable=false>"))

  val nullableTrue = TypeFactory.makeType(parse("<nullable=true>"))

  val regexType = TypeFactory.makeType(parse("<regex='[0-9a-f]+'>"))

  "Values" should "be instances of factory-made Types" in {
    FtanBoolean(true) ==> booleanType
    FtanTrue !=> allType
    FtanNumber(2) ==> anyType
    FtanNumber(2) ==> allType
    FtanNumber(3) !=> allType
    FtanString("") ==> anyType
    FtanNumber(5) ==> minMaxType
    FtanNumber(10) ==> minMaxType
    FtanNumber(11) !=> minMaxType
    FtanNumber(4) !=> minMaxType
    FtanNumber(5) !=> minMaxExclusive
    FtanNumber(10) !=> minMaxExclusive
    FtanNumber(8) ==> minMaxExclusive
    FtanNumber(0) !=> minMaxWithExclusions
    "<test>" ==> nameType
    "<>" !=> nameType
    "<test2>" !=> nameType
    FtanNull !=> nullableFalse
    FtanNull ==> nullableTrue
    FtanFalse ==> nullableFalse
    FtanString("") ==> nullableTrue
    FtanString("03f") ==> regexType
    FtanString("03A") !=> regexType
  }

}