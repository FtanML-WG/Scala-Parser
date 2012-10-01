package ftanml.objects

import java.io.Writer

import scala.collection.mutable.LinkedHashMap
import ftanml.streams.Acceptor

object FtanElement extends FtanElement(new LinkedHashMap[FtanString,FtanValue]) {
  val NAME_KEY = new FtanString("name")
  val CONTENT_KEY = new FtanString("content")
  
  def apply(attributes: (FtanString,FtanValue)*) = new FtanElement(attributes.toMap)
}

case class FtanElement(attributes: LinkedHashMap[FtanString, FtanValue]) extends FtanValue with SizedObject {
  import FtanElement._
  
  def this(attributes: Map[FtanString,FtanValue]) = this(new LinkedHashMap++=attributes)

  def name: Option[String] = {
     attributes.get(NAME_KEY) match {
      case a : Some[FtanValue] => Some(a.get.asInstanceOf[FtanString].value)
      case _ => None
    }
  }

  def content: FtanArray = {
     attributes.get(CONTENT_KEY) match {
      case a : Some[FtanValue] => a.get.asInstanceOf[FtanArray]
      case _ => FtanArray(Nil)
    }
  }

  def isEmptyContent: Boolean = content.values.isEmpty

  def isSimpleContent: Boolean = content.values.size == 1 && content.values(0).isInstanceOf[FtanString]

  def isElementOnlyContent: Boolean = !content.values.exists(!_.isInstanceOf[FtanElement])

  def isMixedContent: Boolean = content.values.exists(_.isInstanceOf[FtanElement]) && content.values.exists(_.isInstanceOf[FtanString])




//  def writeFtanMLContent(writer: Writer) {
//    writeFtanML(writer)
//  }

  override def send(acceptor: Acceptor) {
    acceptor.processStartElement(name)
    for ((key, value) <- attributes) value match {
      //Ignore name attribute, if valid
      case string: FtanString if key == NAME_KEY =>
      //Ignore content attribute, if valid
      case array: FtanArray if key == CONTENT_KEY && array.isValidElementContent =>
      //Write all other attributes
      case value: FtanValue =>
        acceptor.processAttributeName(key.value)
        value.send(acceptor)
    }
    if (!isEmptyContent) {
      acceptor.processStartContent(isElementOnlyContent)
      content.values.foreach {_.send(acceptor)}
    }
    acceptor.processEndElement()
  }

  override def equals(that: Any) =
    that.isInstanceOf[FtanElement] &&
      attributes.size == that.asInstanceOf[FtanElement].attributes.size &&
      attributes.equals(that.asInstanceOf[FtanElement].attributes)


  override def hashCode() : Int = {
    attributes.hashCode()
  }

  def size = {
  	var size = attributes.size
  	
  	//don't want to count name and content as attributes
  	if(attributes.contains(NAME_KEY))
  		size -= 1
  	if(attributes.contains(CONTENT_KEY))
  		size -= 1

  	size
  }
}