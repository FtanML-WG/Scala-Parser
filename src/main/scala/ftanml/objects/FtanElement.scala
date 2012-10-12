package ftanml.objects

import scala.collection.mutable.LinkedHashMap
import ftanml.streams.Acceptor

object FtanElement extends FtanElement(new LinkedHashMap[FtanString,FtanValue]) {
  val NAME_KEY = new FtanString("$name")
  val CONTENT_KEY = new FtanString("$content")
  
  val VALID_NAME = "[\\p{Alpha}\\p{Digit}_:\\$]+".r
  
  def apply(attributes: (FtanString,FtanValue)*) = new FtanElement(attributes.toMap)
}

case class FtanElement(attributes: LinkedHashMap[FtanString, FtanValue]) extends FtanValue with SizedObject {
  import FtanElement._

  /**
   * Create an element with no name, no attributes, and no content
   */
  def this() = this(new LinkedHashMap[FtanString, FtanValue])

  /**
   * Create an element with a specified name, with no attributes and no content
   */

  def this(name: String) = this(new LinkedHashMap[FtanString, FtanValue] += (FtanString("$name") -> FtanString(name)))
  // TODO: the above should use NAME_KEY, but the compiler doesn't like it

  /**
   * Create an element with attributes supplied in the form of a map
   */

  def this(attributes: Map[FtanString,FtanValue]) = this(new LinkedHashMap++=attributes)


  /**
   * Create a new element as a copy of an existing element with a specified name
   */

  def setName(name: String) = setAttribute(NAME_KEY.value, FtanString(name))

  /**
   * Create a new element as a copy of an existing element with an additional or replaced attribute
   */

  def setAttribute(name: String, value: FtanValue) = {
    new FtanElement(attributes += (FtanString(name) -> value))
  }

  /**
   * Create a new element as a copy of an existing element with specified content
   */

  def setContent(value: FtanValue) = setAttribute(CONTENT_KEY.value, value)

  /**
   * Get a named attribute of the element (allows the form element("attname"))
   */

  def apply(attName: FtanString) : FtanValue = attributes.getOrElse(attName, FtanNull)

  /**
   * Get the name of the element. Returns None for an unnamed element
   */

  def name: Option[String] =
     attributes.get(NAME_KEY) map { _.asInstanceOf[FtanString].value }

  /**
   * Get the content of the element
   */

  def content: FtanArray = attributes.get(CONTENT_KEY) map {_.asInstanceOf[FtanArray]} getOrElse FtanArray(Nil)

  /**
   * Ask if the content of the element is empty
   */

  def isEmptyContent: Boolean = content.values.isEmpty

  /**
   * Ask if the content of the element is a single string
   */

  def isSimpleContent: Boolean = content.values.size == 1 && content.values(0).isInstanceOf[FtanString]

  /**
   * Ask if the content of the element contains child elements only
   */

  def isElementOnlyContent: Boolean = !content.values.exists(!_.isInstanceOf[FtanElement])

  /**
   * Ask if the content of the element contains mixed content (at least one element and at least one string)
   */

  def isMixedContent: Boolean = content.values.exists(_.isInstanceOf[FtanElement]) && content.values.exists(_.isInstanceOf[FtanString])

  /**
   * Send the element to an Acceptor
   */

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

  /**
   * Get the size of the element, defined as the number of attributes excluding the name and content
   */

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