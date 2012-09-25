package ftanml.types

import java.util.ArrayList
import ftanml.objects.{FtanArray, FtanString, FtanElement}
import collection.mutable.{ListBuffer, LinkedList}

/**
 * The TypeFactory constructs types from FtanML elements that describe the type
 */

object TypeFactory {

  def makeType(element : FtanElement) : FtanType = {
    val memberTypes = new ListBuffer[FtanType]();
    for (a <- element.attributes) {
      val name = a._1.value
      val value = a._2
      if (name == FtanElement.NAME_KEY.value) {
        val str = value.asInstanceOf[FtanString].value
        if (str == "string") {
          memberTypes += (StringType)
        } else if (str == "number") {
          memberTypes += (NumberType)
        } else if (str == "boolean") {
          memberTypes += (BooleanType)
        } else if (str == "array") {
          memberTypes += (ArrayType)
        } else if (str == "element") {
          memberTypes += (ElementType)
        }
      }
      if (name == "fixed") {
        memberTypes += (new FixedValueType(value))
      } else if (name == "enum") {
        memberTypes += (new EnumerationType(value.asInstanceOf[FtanArray].values));
      } else if (name == "anyOf" || name == "allOf") {
        val componentElements = value.asInstanceOf[FtanArray];
        val componentTypes = new ListBuffer[FtanType];
        for (t <- componentElements.values) {
          val ct = t.asInstanceOf[FtanElement];
          componentTypes += (makeType(ct));
        }
        if (name == "anyOf") {
          memberTypes += (new AnyOfType(componentTypes))
        } else {
          memberTypes += (new AllOfType(componentTypes))
        }
      }
    }
    if (memberTypes.size == 1) {
      memberTypes(0)
    } else {
      new AllOfType(memberTypes)
    }
  }

}