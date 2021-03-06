package ftanml.types

import ftanml.objects.{FtanNull, FtanValue, FtanString, FtanElement}


/**
 * A NameType is a facet applied to elements, that constrains the name of the
 * element to have a particular type. If the type is nullable, then the element
 * name is optional, but if present it must conform to the type given. Common uses
 * are to constrain the name to a particular string (denoted by a FixedValueTest),
 * an enumeration of alternatives, or a regular expression.
 */

class NameType(theType : FtanType) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v: FtanElement => {
        v.name match {
          case Some(n) => FtanString(n).isInstance(theType)
          case None => FtanNull.isInstance(theType)
        }
      }
      case _ => false
    }
  }

  def descriptor = new FtanElement().setAttribute("name", theType.descriptor)
}