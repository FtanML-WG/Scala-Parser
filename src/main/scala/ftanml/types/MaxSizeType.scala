package ftanml.types

import ftanml.objects.{FtanElement, FtanValue, FtanNumber, SizedObject}


class MaxSizeType(size: Int) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : SizedObject => v.size <= size
      case _ => false
    }
  }

  def descriptor = new FtanElement().setAttribute("maxSize", FtanNumber(size))
}