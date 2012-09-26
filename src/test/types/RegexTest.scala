package test.types

import org.scalatest.FlatSpec

import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._

class RegexTest extends FlatSpec {

  val parser = new FtanParser

  def parse(exp : String) : FtanValue = {
    parser.parse(exp)
  }

  "String Values" should "match the regular expression" in {
    assert(FtanString("").isInstance(new RegexType(FtanString(""))), "1")
    assert(FtanString("abc").isInstance(new RegexType(FtanString("abc"))), "2")
    assert(!FtanString("abc").isInstance(new RegexType(FtanString("a"))), "3")
    assert(FtanString("abc").isInstance(new RegexType(FtanString("abc|def"))), "4")
    assert(FtanString("abc").isInstance(new RegexType(FtanString("[a-z]+"))), "5")
  }


}