package test

import org.scalatest.FlatSpec
import java.io.StringReader
import ftanml.util.JsonReader
import org.scalatest.matchers.ShouldMatchers
import java.text.ParseException

class JsonReaderTest extends FlatSpec with ShouldMatchers{
	
	"A JsonReader" must "be able to convert valid JSON to valid FtanML" in {
		JsonReader.convertString("true") should equal ("true")
		JsonReader.convertString("[1, 2.3]") should equal ("[1, 2.3]")
		JsonReader.convertString("{\"att1\":\"val1\", \"att2\":\"val2\"}") should equal ("<\"att1\"=\"val1\" \"att2\"=\"val2\">")
	}
	
	it should "fail on obviously invalid JSON" in {
		intercept[ParseException]{
			JsonReader.convertString("[")
		}
		intercept[ParseException]{
			JsonReader.convertString("{]")
		}
	}
	
	it should "be able to convert even more complex JSON to valid FtanML" in {
		JsonReader.convertString("{\"att1\":[true, false], \"att2\":15.2e5}") should equal ("<\"att1\"=[true, false] \"att2\"=15.2e5>")
		JsonReader.convertString("[null, {\"att1\":null, \"att2\":[10]}, false]") should equal ("[null, <\"att1\"=null \"att2\"=[10]>, false]")
	}
}