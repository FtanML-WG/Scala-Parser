package ftanml.types

import ftanml.objects.{FtanElement, FtanValue, FtanNumber, SizedObject}


class SizeType(size: Int) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v : SizedObject => v.size == size
      case _ => false
    }
  }

  override def descriptor = FtanElement().setAttribute("size", FtanNumber(size))
}