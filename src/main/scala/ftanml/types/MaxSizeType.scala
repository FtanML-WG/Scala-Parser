package ftanml.types

import ftanml.objects.{FtanValue, FtanNumber, GetSize}

class MaxSizeType(size: FtanNumber) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : GetSize => v.getSize <= size.value
      case _ => false
    }
  }
}