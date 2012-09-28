package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class MinMaxTest extends FlatSpec with TypeTest {

  "Numeric Values" should "be in range" in {
    FtanNumber(93.7) ==> new MinValueType(FtanNumber(0), false)
    FtanNumber(93.7) !=> new MaxValueType(FtanNumber(0), false)
    FtanTrue !=> new MinValueType(FtanNumber(100), false)
    FtanNumber(3) ==> new MinValueType(FtanNumber(3), false)
    FtanNumber(3) ==> new MaxValueType(FtanNumber(3), false)
    FtanNumber(3) !=> new MaxValueType(FtanNumber(3), true)
    FtanNumber(3) !=> new MinValueType(FtanNumber(3), true)
  }


}