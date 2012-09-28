package types
import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types._
import util.TypeTest

class ItemTypeTest extends FlatSpec with TypeTest {

	"Arrays" should "be matched correctly by ItemType" in {
		"[true, false]" ==> new ItemType(BooleanType)
		"[true, false, 0]" !=> new ItemType(BooleanType)
		"['test', 0]" ==> new ItemType(new AnyOfType(Seq(StringType, NumberType)))
	}
	
	//doesn't work right now: Element names and content should not be matched, null attributes should be stripped when creating elements
	"Elements" should "be matched correctly by ItemType" ignore {
		"<elem att1=true att2=false>" ==> new ItemType(BooleanType)
		"<elem>" ==> new ItemType(BooleanType)
		"<elem att1=null att2=true>" ==> new ItemType(BooleanType)
		"<elem att1=true att2=false att3=0>" !=> new ItemType(BooleanType)
		"<elem att1='test' att2=0>" ==> new ItemType(new AnyOfType(Seq(StringType, NumberType)))
	}
	
	"Other values" should "not be matched by ItemType" in {
		"true" !=> new ItemType(BooleanType)
		"'test'" != new ItemType(StringType)
	}
}