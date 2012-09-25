package test.types

import org.scalatest.FlatSpec

import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

/**
 * Unit tests for TypeFactory constructing types from elements
 */

class TypeFactoryTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanElement = {
    parser.parse(exp).asInstanceOf[FtanElement]
  }

  var booleanType = TypeFactory.makeType(parse("<boolean>"))

  var threeNumbers = TypeFactory.makeType(parse("<enum=[1,2,3]>"))

  var fixedNumber = TypeFactory.makeType(parse("<fixed=2>"))

  var anyString = TypeFactory.makeType(parse("<string>"));

  var anyNumber = TypeFactory.makeType(parse("<number>"))

  var anyType = TypeFactory.makeType(parse("<anyOf=[<enum=[1,2,3]>, <fixed=2>, <string>]>"))

  var allType = TypeFactory.makeType(parse("<number enum=[1,2,3] fixed=2>"))

  var minMaxType = TypeFactory.makeType(parse("<min=5 max=10>"))

  "Values" should "be instances of factory-made Types" in {
    assert(FtanBoolean(true).isInstance(booleanType), "0")
    assert(!FtanTrue.isInstance(allType), "2")
    assert(FtanNumber(2).isInstance(anyType), "3");
    assert(FtanNumber(2).isInstance(allType), "4");
    assert(!FtanNumber(3).isInstance(allType), "5");
    assert(FtanString("").isInstance(anyType), "6");
    assert(anyType.matches(FtanString("")), "7")
    assert(FtanNumber(5).isInstance(minMaxType), "8")
    assert(FtanNumber(10).isInstance(minMaxType), "9")
    assert(!FtanNumber(11).isInstance(minMaxType), "10")
    assert(!FtanNumber(4).isInstance(minMaxType), "11")
  }


}