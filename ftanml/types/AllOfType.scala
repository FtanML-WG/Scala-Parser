package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the intersection of a set of operand types.
 */

class AllOfType (memberTypes : Seq[FtanType]) extends FtanType {
  // TODO: no tests yet
  def matches(value: FtanValue) = {
    for (f <- memberTypes) {
      if (!f.matches(value)) false
    }
    true
  }
}