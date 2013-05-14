package functions

import org.scalatest.FlatSpec
import util.ParserTest
import ftanml.objects.{FtanTrue, FtanNumber, FtanBoolean}

class FunctionTest extends ParserTest with FlatSpec {
  "OrFunctions" should "work" in {
    "{true || false}" ~~> FtanBoolean(true)
    "{true || true}" ~~> FtanBoolean(true)
    "{false || true}" ~~> FtanBoolean(true)
    "{false || false}" ~~> FtanBoolean(false)
  }

  "AndFunctions" should "work" in {
    "{true && false}" ~~> FtanBoolean(false)
    "{true && true}" ~~> FtanBoolean(true)
    "{false && true}" ~~> FtanBoolean(false)
    "{false && false}" ~~> FtanBoolean(false)
  }

  "Comparisons" should "work" in {
    "{3 = 3}" ~~> FtanBoolean(true)
    "{3 = 4}" ~~> FtanBoolean(false)
    "{false != true}" ~~> FtanBoolean(true)
    "{5 > 3}" ~~> FtanBoolean(true)
    "{5 >= 6}" ~~> FtanBoolean(false)
    "{5 <= 6}" ~~> FtanBoolean(true)
    "{5 != 6}" ~~> FtanBoolean(true)
    "{5 > 6}" ~~> FtanBoolean(false)
  }

  "Arithmetic" should "work" in {
    "{2+2}" ~~> FtanNumber(4)
    "{2 - 1}" ~~> FtanNumber(1)
    "{2 - 1 - 1}" ~~> FtanNumber(0)
    "{2×1.5 + 3}" ~~> FtanNumber(6)
    "{2×1.5 ÷ 3}" ~~> FtanNumber(1)
    "{2×1.5 + 3 = 6}" ~~> FtanTrue
    "{2×1.5 + 3 = 6 && 2 + 2 = 4}" ~~> FtanTrue
    "{8 ÷ 2 ÷ 2}" ~~> FtanNumber(2)
  }

  "Parentheses" should "work" in {
    "{(2)}" ~~> FtanNumber(2)
    "{(true) || (false)}" ~~> FtanTrue
    "{(2+1) × 3}" ~~> FtanNumber(9)
    "{3 × (2+1)}" ~~> FtanNumber(9)
    "{3-(2-1)}" ~~> FtanNumber(2)
    "{(2=3 || 4=2+2) && (5 <= 6)}" ~~> FtanTrue
  }

  "BadFunctions" should "be rejected, if wrong" in {
    "{3" invalid;
    "3}" invalid;
  }

}