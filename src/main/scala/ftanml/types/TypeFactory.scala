package ftanml.types

import collection.mutable.ListBuffer
import ftanml.objects.{ FtanNumber, FtanArray, FtanString, FtanElement, FtanBoolean }

/**
 * The TypeFactory constructs types from FtanML elements that describe the type
 */

object TypeFactory {

  val primitives = collection.immutable.HashMap(
    "string" -> StringType,
    "number" -> NumberType,
    "boolean" -> BooleanType,
    "array" -> ArrayType,
    "element" -> ElementType,
    "any" -> AnyType,
    "nothing" -> NothingType)

  //TODO There are a lot of asInstanceOf that could fail. We should catch these errors and throw an exception.
  def makeType(element: FtanElement): FtanType = {
    val memberTypes = {
      for ((name, value) <- element.attributes) yield {
        name.value match {
          case FtanElement.NAME_KEY.value =>
            val str = value.asInstanceOf[FtanString].value
            primitives.get(str) match {
              case Some(t) => t
              //TODO Better throw an exception than write on stderr. We write a library.
              case _ => sys.error("Unknown primitive type " + str)
            }
          case "fixed" =>
            new FixedValueType(value)
          case "enum" =>
            new EnumerationType(value.asInstanceOf[FtanArray].values)
          case "min" =>
            new MinValueType(value.asInstanceOf[FtanNumber], false)
          case "minExclusive" =>
            new MinValueType(value.asInstanceOf[FtanNumber], true)
          case "max" =>
            new MaxValueType(value.asInstanceOf[FtanNumber], false)
          case "maxExclusive" =>
            new MaxValueType(value.asInstanceOf[FtanNumber], true)
          case "not" =>
            new ComplementType(makeType(value.asInstanceOf[FtanElement]))
          case "nullable" =>
            new NullableType(value.asInstanceOf[FtanBoolean])
          case "regex" =>
            new RegexType(value.asInstanceOf[FtanString])
          case "anyOf" =>
            new AnyOfType(
              value.asInstanceOf[FtanArray].values.map { t =>
                makeType(t.asInstanceOf[FtanElement])
              })
          case "allOf" =>
            new AllOfType(
              value.asInstanceOf[FtanArray].values.map { t =>
                makeType(t.asInstanceOf[FtanElement])
              })
        }
      }
    }

    if(memberTypes.size == 0) {
      AnyType
    } else if (memberTypes.size == 1) {
      memberTypes.head
    } else {
      new AllOfType(memberTypes)
    }
  }

}