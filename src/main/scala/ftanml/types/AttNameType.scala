package ftanml.types

import ftanml.objects.{FtanString, FtanValue, FtanElement}


/**
 * An AttNameType is a facet applied to elements, which constrains the name of the
 * every attribute of the element to have a particular type. It can be used to
 * define an enumeration of permitted attribute names (in which case the element's
 * extensibility is suppressed), or in cases where elements are used as maps, for example
 * from employee numbers to employee data, it can constrain the form of the keys in the map.
 * It could also be used, for example, to restrict attribute names to conform to the XML rules.
 */

class AttNameType(theType : FtanType) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v: FtanElement => v.attributes.keys.forall {(name:String) =>
        name == FtanElement.CONTENT_KEY || FtanString(name).isInstance(theType)}
      case _ => false
    }
  }

  def descriptor = FtanElement().setAttribute("attName", theType.descriptor)
}