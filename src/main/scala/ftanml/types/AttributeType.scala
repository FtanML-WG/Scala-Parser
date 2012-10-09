package ftanml.types

import ftanml.objects.{FtanValue, FtanString, FtanElement}

class AttributeType(name: FtanString, regex: Boolean) extends FtanType {
  def matches(value: FtanValue) = {
    value match {
      case v: FtanElement => v.attributes.contains(FtanElement.NAME_KEY) && (
      		if(regex)
      			("^" + name.value + "$").r.findFirstIn(v.attributes(FtanElement.NAME_KEY).asInstanceOf[FtanString].value) != None
      		else
      			v.attributes(FtanElement.NAME_KEY) == name)
      case _ => false
    }
  }
}