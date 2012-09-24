package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the union of several supplied types
 */

class AnyOfType (memberTypes : Seq[FtanType]) extends FtanType {
  def matches(value: FtanValue) : Boolean = {
    for (f <- memberTypes) {
      if (f.matches(value)) return true
    }
    false
  }
}