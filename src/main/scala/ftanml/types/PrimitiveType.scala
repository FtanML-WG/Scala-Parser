package ftanml.types

import ftanml.objects._


//abstract class PrimitiveType[T] extends FtanType {
//  // TODO: can't get the generic version to work, so overriding it in the subclasses
//  def matches(value: FtanValue) = {
//    value.isInstanceOf[T]
//  }
//}

object StringType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanString]
  }
}

object NumberType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanNumber]
  }
}

object BooleanType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanBoolean]
  }
}

object ArrayType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanArray]
  }
}

object ElementType extends FtanType {
  override def matches(value: FtanValue) = {
    value.isInstanceOf[FtanElement]
  }
}

object NullType extends FtanType {
  override def matches(value: FtanValue) = {
    value == FtanNull
  }
}