package ftanml.types

import ftanml.objects.FtanValue

/**
 * A type that matches any one of a fixed set of permitted values
 */

case class EnumerationType(values : Seq[FtanValue]) extends FtanType {
  def matches(value: FtanValue) = {
    values.contains(value)
  }
}