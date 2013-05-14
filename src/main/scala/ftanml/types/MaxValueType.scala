package ftanml.types

import ftanml.objects.{FtanElement, FtanNumber, FtanValue}


/**
 * A type that matches a value less than or equal to a specified maximum numeric value
 */

class MaxValueType(maxValue : FtanNumber, exclusive : Boolean) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : FtanNumber => {
        var c = v.value.compareTo(maxValue.value)
        c < 0 || (c == 0 && !exclusive)
      }
      case _ => false
    }
  }

  def descriptor = new FtanElement().setAttribute((if (exclusive) "lt" else "le"), maxValue)
}