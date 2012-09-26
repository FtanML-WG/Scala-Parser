package ftanml.types

import ftanml.objects._


abstract class PrimitiveType[T] extends FtanType {
  // TODO: can't get the generic version to work, so overriding it in the subclasses
  def matches(value: FtanValue) = {
    value.isInstanceOf[T]
  }
}

object StringType extends PrimitiveType[FtanString] {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanString]
  }
}

object NumberType extends PrimitiveType[FtanNumber] {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanNumber]
  }
}

object BooleanType extends PrimitiveType[FtanBoolean] {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanBoolean]
  }
}

object ArrayType extends PrimitiveType[FtanArray] {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanArray]
  }
}

object ElementType extends PrimitiveType[FtanElement] {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanElement]
  }
}