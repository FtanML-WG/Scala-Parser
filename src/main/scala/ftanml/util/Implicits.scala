package ftanml.util

import ftanml.objects._

object Implicits {
	
	class NotAnInstanceException(message: String) extends IllegalArgumentException(message)
	
	implicit def FtanValue2FtanNumber(value: FtanValue) = {
		if(value.isInstanceOf[FtanNumber]){
			value.asInstanceOf[FtanNumber]
		}else{
			throw new NotAnInstanceException("Value " + value + " is not an instance of FtanNumber")
		}
	}
	
	implicit def FtanValue2FtanString(value: FtanValue) = {
		if(value.isInstanceOf[FtanString]){
			value.asInstanceOf[FtanString]
		}else{
			throw new NotAnInstanceException("Value " + value + " is not an instance of FtanString")
		}
	}
	
	implicit def FtanValue2FtanArray(value: FtanValue) = {
		if(value.isInstanceOf[FtanList]){
			value.asInstanceOf[FtanList]
		}else{
			throw new NotAnInstanceException("Value " + value + " is not an instance of FtanArray")
		}
	}
	
	implicit def FtanValue2FtanElement(value: FtanValue) = {
		if(value.isInstanceOf[FtanElement]){
			value.asInstanceOf[FtanElement]
		}else{
			throw new NotAnInstanceException("Value " + value + " is not an instance of FtanElement")
		}
	}
}