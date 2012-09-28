package ftanml.types

import ftanml.objects.{FtanValue, FtanString, FtanElement}

class NameType(name: FtanString) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v: FtanElement => v.attributes.contains(FtanElement.NAME_KEY) && v.attributes(FtanElement.NAME_KEY) == name
      case _ => false
    }
  }
}