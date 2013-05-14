package ftanml.types

import ftanml.objects.{FtanList, FtanValue, FtanString, FtanElement}


/**
 * An AttributeType is a facet applied to elements, that constrains the element
 * to have an attribute with a particular name whose value matches a particular type.
 * Since the value of an absent attribute is Null, making the type Nullable effectively
 * makes the attribute optional (but if present, it must still conform to the specified type)
 */

class AttributeType(name: String, attType: FtanType) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v: FtanElement => v(name).isInstance(attType)
      case _ => false
    }
  }

  def descriptor = new FtanElement("element").setContent(FtanList(FtanElement().setAttribute(name, attType.descriptor)))
}