import ftanml.objects.FtanNull
import org.scalatest.FlatSpec

class NullTest extends ParserTest with FlatSpec {  
  
  "Nulls" should "be compared correctly (equals, hashCode)" in {
    FtanNull should_equal FtanNull
    FtanNull() should_equal FtanNull
  }

  they should "be parsed correctly" in {
    "null" <--> FtanNull
  }
  
  they should "be rejected, if wrong" in {
    "Null" invalid;
    "NULL" invalid;
    "nnull" invalid;
    "nulll" invalid;
  }

}