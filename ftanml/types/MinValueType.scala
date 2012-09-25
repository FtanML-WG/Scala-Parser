package ftanml.types

import ftanml.objects.{FtanNumber, FtanValue}


/**
 * A type that matches a value greater than or equal to a specified minimum numeric value
 */

class MinValueType(minValue : FtanNumber) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : FtanNumber => v.value >= minValue.value
      case _ => false
    }
  }
}