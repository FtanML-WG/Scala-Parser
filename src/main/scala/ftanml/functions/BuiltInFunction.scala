package ftanml.functions

import ftanml.exec.Context
import ftanml.streams.Acceptor
import ftanml.types._
import ftanml.objects._


/**
 * Created by IntelliJ IDEA.
 * User: mike
 * Date: 14/05/2013
 * Time: 11:48
 * To change this template use File | Settings | File Templates.
 */

abstract class BuiltInFunction(name: String, argTypes: Seq[FtanType], resultType: FtanType) extends FtanValue with FtanFunction {
  def send(acceptor: Acceptor) {}
  def call(context: Context, args: Seq[FtanValue]): FtanValue;
  def apply(context: Context, args: Seq[FtanValue]) = {
    if (args.size != argTypes.size) {
      throw new IllegalArgumentException("Function " + name + " expects " + argTypes.size + " arguments, not " + args.size + " as supplied")
    }
    List.forall2(args.toList, argTypes.toList) {
      (v: FtanValue, t: FtanType) =>
      if (v.isInstance(t)) true
      else throw new IllegalArgumentException("Argument " + v + " to function " + name + " is not of type " + t)
    }
    var result = call(context, args)
    if (result.isInstance(resultType)) {
      result
    } else {
      throw new IllegalArgumentException("Result of function " + name + " is " + result + ", which does not match the required type " + resultType)
    }
  }
}

object eq extends BuiltInFunction("eq", List(AnyType, AnyType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).equals(args(1)))
  }
}

object ne extends BuiltInFunction("ne", List(AnyType, AnyType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(!args(0).equals(args(1)))
  }
}

object le extends BuiltInFunction("le", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) <= 0)
  }
}

object lt extends BuiltInFunction("lt", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) < 0)
  }
}

object ge extends BuiltInFunction("ge", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) >= 0)
  }
}

object gt extends BuiltInFunction("gt", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) > 0)
  }
}

object plus extends BuiltInFunction("plus", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.add(args(1).asInstanceOf[FtanNumber].value))
  }
}

object minus extends BuiltInFunction("minus", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.subtract(args(1).asInstanceOf[FtanNumber].value))
  }
}

object times extends BuiltInFunction("times", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.multiply(args(1).asInstanceOf[FtanNumber].value))
  }
}

object div extends BuiltInFunction("div", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.divide(args(1).asInstanceOf[FtanNumber].value))
  }
}

object subscript extends BuiltInFunction("itemAt", List(ListType, NumberType), AnyType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList](args(1).asInstanceOf[FtanNumber].toInt)
  }
}

object map extends BuiltInFunction("map", List(ListType, FunctionType), ListType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList].map(context, args(1).asInstanceOf[FtanFunction])
  }
}

object filter extends BuiltInFunction("filter", List(ListType, FunctionType), ListType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList].filter(context, args(1).asInstanceOf[FtanFunction])
  }
}

object attribute extends BuiltInFunction("attribute", List(ElementType, StringType), ValueType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanElement](args(1).asInstanceOf[FtanString].value)
  }
}