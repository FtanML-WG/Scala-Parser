package ftanml.types

import ftanml.objects.{FtanElement, FtanValue}


trait FtanType {

  def matches (value : FtanValue) : Boolean

  def descriptor : FtanElement

  override def toString = descriptor.toFtanML

}