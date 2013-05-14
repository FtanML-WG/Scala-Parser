package ftanml.types

import ftanml.objects.{FtanElement, FtanNumber, FtanValue}


/**
 * A type that matches a value greater than or equal to a specified minimum numeric value
 */

class MinValueType(minValue : FtanNumber, exclusive : Boolean) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : FtanNumber => {
        var c = v.value.compareTo(minValue.value)
        c > 0 || (c == 0 && !exclusive)
      }
      case _ => false
    }
  }

  def descriptor = new FtanElement().setAttribute((if (exclusive) "gt" else "ge"), minValue)
}