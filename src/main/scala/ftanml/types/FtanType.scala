package ftanml.types

import ftanml.objects.FtanValue


abstract class FtanType {

  def matches (value : FtanValue) : Boolean

}