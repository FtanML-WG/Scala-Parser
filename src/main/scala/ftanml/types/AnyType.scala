package ftanml.types

import ftanml.objects.{FtanValue, FtanElement}


/**
 * A type that matches any value
 */
object AnyType extends FtanType {

  def matches(value: FtanValue) = true

  override def descriptor = new FtanElement("any")

}