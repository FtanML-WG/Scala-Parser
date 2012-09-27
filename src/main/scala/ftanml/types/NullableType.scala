package ftanml.types

import ftanml.objects.{FtanNull, FtanBoolean, FtanValue}

class NullableType(nullable: FtanBoolean) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case FtanNull => nullable.value
      case _ => true
    }
  }
}