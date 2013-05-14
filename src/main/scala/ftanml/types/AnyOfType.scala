package ftanml.types

import ftanml.objects.{FtanList, FtanElement, FtanValue}


/**
 * A type that matches the union of several supplied types
 */

class AnyOfType (memberTypes : Traversable[FtanType]) extends FtanType {
  
  def matches(value: FtanValue) : Boolean = memberTypes.exists{_.matches(value)}

  def descriptor = new FtanElement("anyOf").setContent(FtanList(List.concat(memberTypes.map(_.descriptor))))
}