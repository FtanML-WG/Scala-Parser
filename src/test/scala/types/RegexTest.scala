package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.FtanParser
import ftanml.types._
import util.TypeTest

class RegexTest extends FlatSpec with TypeTest {
  
  "String Values" should "match the regular expression" in {
    FtanString("") ==> new RegexType(FtanString(""))
    FtanString("abc") ==> new RegexType(FtanString("abc"))
    FtanString("abc") !=> new RegexType(FtanString("a"))
    FtanString("abc") ==> new RegexType(FtanString("abc|def"))
    FtanString("abc") ==> new RegexType(FtanString("[a-z]+"))
  }


}