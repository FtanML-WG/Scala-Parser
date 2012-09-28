package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches any one of a fixed set of permitted values
 */

case class EnumerationType(values : Traversable[FtanValue]) extends FtanType {
  
  def matches(value: FtanValue) = values.exists(_==value)
}