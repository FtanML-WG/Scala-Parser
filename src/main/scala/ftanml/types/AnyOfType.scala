package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the union of several supplied types
 */

class AnyOfType (memberTypes : Traversable[FtanType]) extends FtanType {
  
  def matches(value: FtanValue) : Boolean = memberTypes.exists{_.matches(value)}
}