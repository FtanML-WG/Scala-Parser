package ftanml.util

import java.io.Reader
import java.util.Stack
import java.text.ParseException
import java.io.StringReader

object JsonReader {
	def convertString(string: String) : String = {
		val char: Array[Char] = new Array[Char](string.length)
		new JsonReader(new StringReader(string)).read(char)
		String.valueOf(char).trim()
	}
}

class JsonReader(base: Reader) extends Reader {
	private var depthMapsArray = new Stack[Int] //0: Map, 1: Array (tried to do it with an enum but couldn't get that working
	private var escaped = false
	private var inString = false
	private var offset = 0
	
	override def read: Int = {
		val char = base.read
		offset += 1
		char match {
			case '"' => if(!escaped) inString = !inString; '"'
			case '\\' => if(inString) escaped = !escaped; '\\'
			case _ if(inString) => char
			case '{' => depthMapsArray.push(0); '<'
			case '}' => if(depthMapsArray.pop != 0) throw new ParseException("Expected ], found }", offset-1); '>'
			case '[' => depthMapsArray.push(1); '['
			case ']' => if(depthMapsArray.pop != 1) throw new ParseException("Expected }, found ]", offset-1); ']'
			case ':' => '='
			case ',' if(depthMapsArray.peek == 0) => read
			case -1 => if(depthMapsArray.size != 0) throw new ParseException("End of file reached, but map or array unclosed", offset-1); -1
			case _ => char
		}
	}
	
	override def read(cbuf: Array[Char], off: Int = 0, len: Int = -1): Int = {
		val length = if(len == -1) cbuf.length else math.min(len, cbuf.length)
		var i = 0
		
		base.skip(off)
		offset += off
		
		var char = read
		
		if (char == -1) return -1
		
		while(i < length && char != -1){
			cbuf(i) = char.asInstanceOf[Char]
			char = read
			i += 1
		}
			
		i + 1
	}

	def close: Unit = {base.close}

}