package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the intersection of a set of operand types.
 */

class AllOfType (memberTypes : Seq[FtanType]) extends FtanType {
  def matches(value: FtanValue) : Boolean = {
    for (f <- memberTypes) {
      if (!f.matches(value)) return false
    }
    true
  }
}