package ftanml.types

import ftanml.objects.FtanValue


/**
 * A type that matches a value if and only if some other type does NOT match the value
 */

class ComplementType(base : FtanType) extends FtanType {
  def matches(value: FtanValue) = {
    !base.matches(value)
  }
}