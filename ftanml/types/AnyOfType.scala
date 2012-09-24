package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches the union of several supplied types
 */

class AnyOfType (memberTypes : Seq[FtanType]) extends FtanType {
  // TODO: no tests yet
  def matches(value: FtanValue) = {
    for (f <- memberTypes) {
      if (f.matches(value)) true
    }
    false
  }
}