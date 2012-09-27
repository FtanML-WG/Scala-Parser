package ftanml.util

import java.io.Reader
import java.util.Stack

class JsonReader(base: Reader) extends Reader {
	private var depthMapsArray = new Stack[Int] //0: Map, 1: Array (tried to do it with an enum but couldn't get that working
	private var escaped = false
	private var inString = false
	
	override def read: Int = {
		val char = base.read
		char match {
			case '"' => if(!escaped) inString = !inString; '"'
			case '\\' => if(inString) escaped = !escaped; '\\'
			case _ if(inString) => char
			case '{' => depthMapsArray.push(0); '<'
			case '}' => assert(depthMapsArray.pop == 0, "Expected ], found }"); '>'
			case '[' => depthMapsArray.push(1); '['
			case ']' => assert(depthMapsArray.pop == 1, "Expected }, found ]"); ']'
			case ':' => '='
			case ',' if(depthMapsArray.peek == 0) => read
			case -1 => assert(depthMapsArray.size == 0, "End of file reached, but map or array unclosed"); -1
			case _ => char
		}
	}
	
	def read(cbuf: Array[Char], off: Int = 0, len: Int = -1): Int = {
		val length = if(len == -1) cbuf.length else math.min(len, cbuf.length)
		var i = 0
		
		base.skip(off)
		
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