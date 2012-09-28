package ftanml.types

import ftanml.objects.FtanValue


trait FtanType {

  def matches (value : FtanValue) : Boolean

}