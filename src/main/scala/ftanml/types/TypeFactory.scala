package ftanml.types

import collection.mutable.ListBuffer
import ftanml.objects.{FtanNumber, FtanArray, FtanString, FtanElement}

/**
 * The TypeFactory constructs types from FtanML elements that describe the type
 */

object TypeFactory {

  val primitives = collection.immutable.HashMap (
    "string" -> StringType,
    "number" -> NumberType,
    "boolean" -> BooleanType,
    "array" -> ArrayType,
    "element" -> ElementType
  )

  def makeType(element : FtanElement) : FtanType = {
    val memberTypes = new ListBuffer[FtanType]();
    for (a <- element.attributes) {
      val name = a._1.value
      val value = a._2
      name match {
        case FtanElement.NAME_KEY.value =>
          val str = value.asInstanceOf[FtanString].value
          primitives.get(str) match {
            case Some(t) => memberTypes += t
            case _ => sys.error ("Unknown primitive type " + str)
          }
        case "fixed" =>
          memberTypes += new FixedValueType(value)
        case "enum" =>
          memberTypes += new EnumerationType(value.asInstanceOf[FtanArray].values)
        case "min" =>
          memberTypes += new MinValueType(value.asInstanceOf[FtanNumber], false)
        case "minExclusive" =>
          memberTypes += new MinValueType(value.asInstanceOf[FtanNumber], true)
        case "max" =>
          memberTypes += new MaxValueType(value.asInstanceOf[FtanNumber], false)
        case "maxExclusive" =>
          memberTypes += new MaxValueType(value.asInstanceOf[FtanNumber], true)
        case "not" =>
          val base = value.asInstanceOf[FtanElement]
          memberTypes += new ComplementType(makeType(base))
        case "regex" =>
          memberTypes += new RegexType(value.asInstanceOf[FtanString])
        case "anyOf" =>
          val componentElements = value.asInstanceOf[FtanArray];
          val componentTypes = new ListBuffer[FtanType];
          for (t <- componentElements.values) {
            val ct = t.asInstanceOf[FtanElement];
            componentTypes += (makeType(ct));
          }
          memberTypes += new AnyOfType(componentTypes)
        case "allOf" =>
          val componentElements = value.asInstanceOf[FtanArray];
          val componentTypes = new ListBuffer[FtanType];
          for (t <- componentElements.values) {
            val ct = t.asInstanceOf[FtanElement];
            componentTypes += (makeType(ct));
          }
          memberTypes += new AllOfType(componentTypes)
      }
    }
    if (memberTypes.size == 1) {
      memberTypes(0)
    } else {
      new AllOfType(memberTypes)
    }
  }

}