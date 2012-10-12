package ftanml.types

import ftanml.objects.FtanElement

/**
 * A type that don't matches any value
 */
object NothingType extends AnyOfType(Nil) {
  override def descriptor = new FtanElement("none")
}