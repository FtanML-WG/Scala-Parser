package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the intersection of a set of operand types.
 */

class AllOfType (memberTypes : Traversable[FtanType]) extends FtanType {
  def matches(value: FtanValue) : Boolean = memberTypes.forall{_.matches(value)}
}