package ftanml.types

import ftanml.objects.{FtanList, FtanElement, FtanValue}


/**
 * A type that matches the intersection of a set of operand types.
 */

class AllOfType (memberTypes : Traversable[FtanType]) extends FtanType {
  
  def matches(value: FtanValue) : Boolean = memberTypes.forall{_.matches(value)}

  def descriptor = new FtanElement("allOf").setContent(FtanList(List.concat(memberTypes.map(_.descriptor))))
}