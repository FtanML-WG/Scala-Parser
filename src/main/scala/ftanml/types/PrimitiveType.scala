package ftanml.types

import ftanml.objects._

object ValueType extends FtanType {
  override def matches(value: FtanValue) = {
    true
  }
  override def descriptor = new FtanElement("value")
}

object StringType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanString]
  }
  override def descriptor = new FtanElement("string")
}

object NullableStringType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanString] || value == FtanNull
  }
  override def descriptor = new FtanElement("nullable").setContent(new FtanElement("string"))
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

object ListType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanList]
  }
  override def descriptor = new FtanElement("list")
}

object ElementType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanElement]
  }
  override def descriptor = new FtanElement("element")
}

object TextType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanText]
  }
  override def descriptor = new FtanElement("text")
}

object NullType extends FtanType {
  override def matches(value: FtanValue) = {
    value == FtanNull
  }
  override def descriptor = new FtanElement("null")
}

object FunctionType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanFunction]
  }
  override def descriptor = new FtanElement("function")
}