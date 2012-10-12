package ftanml.types

import ftanml.objects.{FtanArray, FtanElement, FtanValue}
import collection.mutable.LinkedList


/**
 * A type that matches the intersection of a set of operand types.
 */

class AllOfType (memberTypes : Traversable[FtanType]) extends FtanType {
  
  def matches(value: FtanValue) : Boolean = memberTypes.forall{_.matches(value)}

  def descriptor = new FtanElement("allOf").setContent(FtanArray(List.concat(memberTypes.map(_.descriptor))))
}