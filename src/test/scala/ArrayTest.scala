import org.scalatest.FlatSpec
import ftanml.objects.FtanNull
import ftanml.objects.FtanArray
import ftanml.objects.FtanString
import ftanml.objects.FtanNumber
import ftanml.objects.FtanBoolean

class ArrayTest extends ParserTest with FlatSpec {

  "Arrays" should "be compared correctly (equals, hashCode)" in {
    FtanArray() should_equal FtanArray()
    FtanArray(FtanString("bla")) should_equal FtanArray(FtanString("bla"))
    FtanArray(FtanNumber(2.34)) should_equal FtanArray(FtanNumber(2.34))
    FtanArray(FtanBoolean(true), FtanNull) should_equal FtanArray(FtanBoolean(true), FtanNull)
    FtanArray(FtanBoolean(true), FtanNull) should_not_equal FtanArray(FtanBoolean(true))
    FtanArray(FtanBoolean(true), FtanNull) should_not_equal FtanArray(FtanBoolean(true), FtanNull, FtanNull)
    FtanArray(FtanBoolean(true), FtanNull) should_not_equal FtanArray(FtanBoolean(true), FtanNumber(0))
    FtanArray(FtanBoolean(true), FtanNull) should_not_equal FtanArray(FtanBoolean(false), FtanNull)
    FtanArray(FtanString("1")) should_not_equal FtanArray(FtanNumber(1))
    FtanArray(FtanString("1.0")) should_not_equal FtanArray(FtanNumber(1))
    FtanArray(FtanArray(FtanArray()), FtanArray()) should_not_equal FtanArray(FtanArray(FtanArray()), FtanArray(FtanArray()))
    FtanArray(FtanArray(FtanArray()), FtanArray()) should_not_equal FtanArray(FtanArray(), FtanArray())
    FtanArray(FtanArray(FtanArray()), FtanArray()) should_not_equal FtanArray(FtanArray(), FtanArray(FtanArray()))
  }

  they should "be parsed correctly" in {
    "[]" <--> FtanArray()
    "[\"bla\"]" <--> FtanArray(FtanString("bla"))
    "[2.34]" <--> FtanArray(FtanNumber(2.34))
    "[2.34,23.4]" <-- FtanArray(FtanNumber(2.34), FtanNumber(23.4)) <-- "[2.34,2.34e1]"
    "[1.23,'bl\"a',false,null,[1.0,null]]" <--
      FtanArray(FtanNumber(1.23), FtanString("bl\"a"), FtanBoolean(false), FtanNull, FtanArray(FtanNumber(1), FtanNull)) <-- (
        "[1.23,'bl\"a',false,null,[1.0,null]]",
        "[12.3e-1,\"bl\\\"a\",false,null,[1e+0,null]]")
    "[[[1.2]]]" <--> FtanArray(FtanArray(FtanArray(FtanNumber(1.2))))
    "[[[]],[]]" <-- FtanArray(FtanArray(FtanArray()), FtanArray()) <-- (
      "[[[]],[]]", " [ [ [ ] ] , [ ] ] ")
    "[null,[0.1,[\"0.2\"],true],false,[0.5],0.6]" <--
      FtanArray(FtanNull, FtanArray(FtanNumber(0.1), FtanArray(FtanString("0.2")), FtanBoolean(true)), FtanBoolean(false), FtanArray(FtanNumber(0.5)), FtanNumber(0.6)) <--
      ("[null,[0.1,[\"0.2\"],true],false,[0.5],0.6]", "[null, [1e-1 ,['0.2' ] ,true],false,[0.05e1],0.06E+1]")
  }

  they should "have a default value when used directly" in {
    FtanArray should_equal FtanArray()
    FtanArray() should_equal FtanArray(Seq())
  }

  they should "be rejected, if wrong" in {
    //TODO
  }

}