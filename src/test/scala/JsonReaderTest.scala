package test.scala

import org.scalatest.FlatSpec
import java.io.StringReader
import ftanml.util.JsonReader
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class JsonReaderTest extends FlatSpec with ShouldMatchers{
	
	"A JsonReader" must "be able to convert valid JSON to valid FtanML" in {
		convertString("true") should equal ("true")
		convertString("[1, 2.3]") should equal ("[1, 2.3]")
		convertString("{\"att1\":\"val1\", \"att2\":\"val2\"}") should equal ("<\"att1\"=\"val1\" \"att2\"=\"val2\">")
	}
	
	it should "fail on obviously invalid JSON" in {
		intercept[AssertionError]{
			convertString("[")
		}
		intercept[AssertionError]{
			convertString("{]")
		}
	}
	
	it should "be able to convert even more complex JSON to valid FtanML" in {
		convertString("{\"att1\":[true, false], \"att2\":15.2e5}") should equal ("<\"att1\"=[true, false] \"att2\"=15.2e5>")
		convertString("[null, {\"att1\":null, \"att2\":[10]}, false]") should equal ("[null, <\"att1\"=null \"att2\"=[10]>, false]")
	}
	
	def convertString(string: String): String = {
		val reader: JsonReader = new JsonReader(new StringReader(string))
		val char: Array[Char] = new Array[Char](string.length)
		reader.read(char)
		String.valueOf(char).trim()
	}

}