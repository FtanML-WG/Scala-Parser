package ftanml.types

import ftanml.objects.FtanElement

/**
 * A type that doesn't match any value
 */
object NothingType extends AnyOfType(Nil) {
  override def descriptor = new FtanElement("nothing")
}