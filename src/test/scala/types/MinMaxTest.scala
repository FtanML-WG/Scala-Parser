package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

class MinMaxTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "Numeric Values" should "be in range" in {
    assert(FtanNumber(93.7).isInstance(new MinValueType(FtanNumber(0), false)), "1")
    assert(!FtanNumber(93.7).isInstance(new MaxValueType(FtanNumber(0), false)), "2")
    assert(!FtanTrue.isInstance(new MinValueType(FtanNumber(100), false)), "3")
    assert(new MinValueType(FtanNumber(3), false).matches(FtanNumber(3)), "4")
    assert(new MaxValueType(FtanNumber(3), false).matches(FtanNumber(3)), "5")
    assert(!new MaxValueType(FtanNumber(3), true).matches(FtanNumber(3)), "6")
    assert(!new MinValueType(FtanNumber(3), true).matches(FtanNumber(3)), "7")
  }


}