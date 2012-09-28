package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches any value
 */
object AnyType extends FtanType {
  
	def matches (value : FtanValue) = true
}