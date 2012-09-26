package ftanml.types

import ftanml.objects.{FtanNumber, FtanValue}


/**
 * A type that matches a value greater than or equal to a specified minimum numeric value
 */

class MinValueType(minValue : FtanNumber, exclusive : Boolean) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : FtanNumber => if(exclusive) v.value > minValue.value else v.value >= minValue.value
      case _ => false
    }
  }
}