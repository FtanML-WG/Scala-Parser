package ftanml.types

import ftanml.objects.FtanValue


/**
 * A type that only matches one fixed value
 */

class FixedValueType(fixedValue : FtanValue) extends FtanType {
  
  def matches(value: FtanValue) = value == fixedValue
}