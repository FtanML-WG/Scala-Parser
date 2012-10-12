package ftanml.types

import ftanml.objects.{FtanString, FtanElement}


/**
 * A type that matches any value
 */
object AnyType extends AllOfType(Nil) {

  override def descriptor = new FtanElement("any")

}