package objects

import org.scalatest.FlatSpec
import util.ParserTest
import ftanml.objects._
import ftanml.objects.FtanList._

class ListTest extends ParserTest with FlatSpec {

  "Lists" should "be compared correctly (equals, hashCode)" in {
    FtanList() should_equal FtanList()
    FtanList(FtanString("bla")) should_equal FtanList(FtanString("bla"))
    FtanList(FtanNumber(2.34)) should_equal FtanList(FtanNumber(2.34))
    FtanList(FtanBoolean(true), FtanNull) should_equal FtanList(FtanBoolean(true), FtanNull)
    FtanList(FtanBoolean(true), FtanNull) should_not_equal FtanList(FtanBoolean(true))
    FtanList(FtanBoolean(true), FtanNull) should_not_equal FtanList(FtanBoolean(true), FtanNull, FtanNull)
    FtanList(FtanBoolean(true), FtanNull) should_not_equal FtanList(FtanBoolean(true), FtanNumber(0))
    FtanList(FtanBoolean(true), FtanNull) should_not_equal FtanList(FtanBoolean(false), FtanNull)
    FtanList(FtanString("1")) should_not_equal FtanList(FtanNumber(1))
    FtanList(FtanString("1.0")) should_not_equal FtanList(FtanNumber(1))
    FtanList(FtanList(FtanList()), FtanList()) should_not_equal FtanList(FtanList(FtanList()), FtanList(FtanList()))
    FtanList(FtanList(FtanList()), FtanList()) should_not_equal FtanList(FtanList(), FtanList())
    FtanList(FtanList(FtanList()), FtanList()) should_not_equal FtanList(FtanList(), FtanList(FtanList()))
  }

  "Lists2" should "be parsed correctly" in {
    "[]" <--> FtanList()
    "[\"bla\"]" <--> FtanList(FtanString("bla"))
    "[2.34]" <--> FtanList(FtanNumber(2.34))
    "[2.34,23.4]" <-- FtanList(FtanNumber(2.34), FtanNumber(23.4)) <-- "[2.34,2.34e1]"
    "[1.23,'bl\"a',false,null,[1,null]]" <--
      FtanList(FtanNumber(1.23), FtanText(FtanString("bl\"a")), FtanBoolean(false), FtanNull, FtanList(FtanNumber(1), FtanNull)) <-- (
        "[1.23,'bl\"a',false,null,[1.0,null]]")
    "[[[1.2]]]" <--> FtanList(FtanList(FtanList(FtanNumber(1.2))))
    "[[[]],[]]" <-- FtanList(FtanList(FtanList()), FtanList()) <-- (
      "[[[]],[]]", " [ [ [ ] ] , [ ] ] ")
    "[null,[0.1,[\"0.2\"],true],false,[0.5],0.6]" <--
      FtanList(FtanNull, FtanList(FtanNumber(0.1), FtanList(FtanString("0.2")), FtanBoolean(true)), FtanBoolean(false), FtanList(FtanNumber(0.5)), FtanNumber(0.6)) <--
      ("[null,[0.1,[\"0.2\"],true],false,[0.5],0.6]", "[null, [1e-1 ,[\"0.2\" ] ,true],false,[0.05e1],0.06E+1]")
  }

  "Lists3" should "have a default value when used directly" in {
    FtanList should_equal FtanList()
    FtanList() should_equal FtanList(Seq())
  }

  "Lists4" should "compare correctly, when parsed" in {
    TestParser.parsing("[1,2,3]") should_equal TestParser.parsing("[ 1 2 3 ]")
    TestParser.parsing("[1,]") should_equal TestParser.parsing("[ 1, null ]")
    TestParser.parsing("[,1]") should_equal TestParser.parsing("[ null, 1 ]")
    TestParser.parsing("[,,]") should_equal TestParser.parsing("[ null null null ]")
    TestParser.parsing("[<e><f>]") should_equal TestParser.parsing("[ < e >, < f > ]")
    TestParser.parsing("[]") should_not_equal TestParser.parsing("[ null ]")
  }

  "Lists9" should "be rejected, if wrong" in {
    "[" invalid;
    "[]]" invalid;
  }

}