package ftanml.types

import ftanml.objects._

object StringType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanString]
  }
  override def descriptor = new FtanElement("string")
}

object NumberType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanNumber]
  }
  override def descriptor = new FtanElement("number")
}

object BooleanType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanBoolean]
  }
  override def descriptor = new FtanElement("boolean")
}

object ArrayType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanArray]
  }
  override def descriptor = new FtanElement("array")
}

object ElementType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanElement]
  }
  override def descriptor = new FtanElement("element")
}

object NullType extends FtanType {
  override def matches(value: FtanValue) = {
    value == FtanNull
  }
  override def descriptor = new FtanElement("null")
}