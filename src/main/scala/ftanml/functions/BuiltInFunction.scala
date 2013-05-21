package ftanml.functions

import ftanml.exec.Context
import ftanml.streams.Acceptor
import ftanml.types._
import ftanml.objects._


/**
 * Implementations of functions that correspond to operators in the FtanSkrit grammar
 */

abstract class BuiltInFunction(name: String, argTypes: Seq[FtanType], resultType: FtanType) extends FtanValue with FtanFunction {
  def send(acceptor: Acceptor) {}
  def call(context: Context, args: Seq[FtanValue]): FtanValue;
  def apply(context: Context, args: Seq[FtanValue]) = {
    if (args.size != argTypes.size) {
      throw new IllegalArgumentException("Function " + name + " expects " + argTypes.size + " arguments, not " + args.size + " as supplied")
    }
    (args.toList, argTypes.toList).zipped.foreach { (v: FtanValue, t: FtanType) =>
      if (!v.isInstance(t))
      throw new IllegalArgumentException("Argument " + v + " to function " + name + " is not of type " + t)
    }
    var result = call(context, args)
    if (result.isInstance(resultType)) {
      result
    } else {
      throw new IllegalArgumentException("Result of function " + name + " is " + result + ", which does not match the required type " + resultType)
    }
  }
}

/**
 * abs(N) returns the absolute value of a number N
 */

object abs extends BuiltInFunction("attribute", List(NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.abs())
  }
}

/**
 * attribute(E, N) gets the value of the attribute of element E named N, or FtanNull if absent
 */

object attribute extends BuiltInFunction("attribute", List(ElementType, StringType), ValueType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanElement](args(1).asInstanceOf[FtanString].value)
  }
}

/**
 * contains(S, T) returns true if string T is a substring of string S
 */

object contains extends BuiltInFunction("attribute", List(StringType, StringType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanString].value.contains(args(1).asInstanceOf[FtanString].value))
  }
}

/**
 * content(E) returns the content of element E, or FtanNull if the element is unnamed
 */

object content extends BuiltInFunction("content", List(ElementType), ValueType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanElement]("")
  }
}

/**
 * div(N, D) divides N by D
 */

object div extends BuiltInFunction("div", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.divide(args(1).asInstanceOf[FtanNumber].value))
  }
}

/**
 * endsWith(S, T) returns true if string T is a suffix of string S
 */

object endsWith extends BuiltInFunction("attribute", List(StringType, StringType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanString].value.endsWith(args(1).asInstanceOf[FtanString].value))
  }
}

/**
 * eq(V, W) tests if values V and W are equal
 */

object eq extends BuiltInFunction("eq", List(AnyType, AnyType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).equals(args(1)))
  }
}

/**
 * filter(L, F) filters a list L selecting those members for which function F returns true
 */

object filter extends BuiltInFunction("filter", List(ListType, FunctionType), ListType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList].filter(context, args(1).asInstanceOf[FtanFunction])
  }
}

/**
 * ge(M, N) returns true if number M is greater than or equal to number N
 */

object ge extends BuiltInFunction("ge", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) >= 0)
  }
}

/**
 * gt(M, N) returns true if number M is greater than number N
 */

object gt extends BuiltInFunction("gt", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) > 0)
  }
}

/**
 * item(L, N) returns the Nth item of list L, zero-based, or FtanNull if out of range
 */

object item extends BuiltInFunction("item", List(ListType, NumberType), AnyType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList](args(1).asInstanceOf[FtanNumber].toInt)
  }
}

/**
 * le(M, N) returns true if number M is less than or equal to number N
 */

object le extends BuiltInFunction("le", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) <= 0)
  }
}

/**
 * lt(M, N) returns true if number M is less than number N
 */

object lt extends BuiltInFunction("lt", List(NumberType, NumberType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanNumber].value.compareTo(args(1).asInstanceOf[FtanNumber].value) < 0)
  }
}

/**
 * map(L, F) applies function F to every item in list L
 */

object map extends BuiltInFunction("map", List(ListType, FunctionType), ListType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanList].map(context, args(1).asInstanceOf[FtanFunction])
  }
}

/**
 * minus(M, N) subtracts number N from M
 */

object minus extends BuiltInFunction("minus", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.subtract(args(1).asInstanceOf[FtanNumber].value))
  }
}

/**
 * name(E) returns the name of element E, or FtanNull if the element is unnamed
 */

object name extends BuiltInFunction("name", List(ElementType), NullableStringType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    args(0).asInstanceOf[FtanElement].name match {
      case Some(s) => FtanString(s)
      case None => FtanNull
    }
  }
}

/**
 * ne(V, W) returns true if values V and W are not equal
 */

object ne extends BuiltInFunction("ne", List(AnyType, AnyType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(!args(0).equals(args(1)))
  }
}

/**
 * plus(M, N) adds numbers M and N
 */

object plus extends BuiltInFunction("plus", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.add(args(1).asInstanceOf[FtanNumber].value))
  }
}

/**
 * startsWith(S, T) returns true if string T is a prefix of string S
 */

object startsWith extends BuiltInFunction("attribute", List(StringType, StringType), BooleanType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanBoolean(args(0).asInstanceOf[FtanString].value.startsWith(args(1).asInstanceOf[FtanString].value))
  }
}


/**
 * times(M, N) multiplies numbers M and N
 */

object times extends BuiltInFunction("times", List(NumberType, NumberType), NumberType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanNumber(args(0).asInstanceOf[FtanNumber].value.multiply(args(1).asInstanceOf[FtanNumber].value))
  }
}

/**
 * to(M, N) delivers all integers between M and N inclusive
 */

object to extends BuiltInFunction("to", List(IntegerType, IntegerType), ListType) {
  def call(context: Context, args: Seq[FtanValue]) = {
    FtanList(List.range(args(0).asInstanceOf[FtanNumber].toInt, args(1).asInstanceOf[FtanNumber].toInt + 1).map{
      FtanNumber(_)
    })
  }
}
