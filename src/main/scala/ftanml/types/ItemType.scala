package ftanml.types

import ftanml.objects._

class ItemType(itemtype: FtanType) extends FtanType {
  
  def matches(value: FtanValue) = {
    value match {
      case v : FtanArray => v.values.forall(_.isInstance(itemtype))
      case v : FtanElement => v.attributes.values.forall(_.isInstance(itemtype))
      case _ => false
    }
  }
}