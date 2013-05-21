package ftanml.objects

import ftanml.streams.Acceptor

object FtanElement extends FtanElement(Map[String,  FtanValue]()) {

  val CONTENT_KEY = ""

  val VALID_NAME = "\\p{Alpha}[\\p{Alpha}\\p{Digit}_:\\$]*".r

  def apply() = new FtanElement()

  override def apply(name: String) = new FtanElement(name)

  def apply(attributes: (String,FtanValue)*) = new FtanElement(attributes.toMap.filter(_._2 != FtanNull))

  def apply(attributes: Map[String,  FtanValue]) = new FtanElement(attributes.filter(_._2 != FtanNull))

  def apply(name: String, attributes: (String,FtanValue)*) = new FtanElement(attributes.toMap.filter(_._2 != FtanNull)).setName(name)

  def apply(name: String, attributes: Map[String,  FtanValue]) = new FtanElement(name, attributes.filter(_._2 != FtanNull))
}

/**
 * If the default constructor is used, the attribute map must not include any attributes whose value is FtanNull.
 */

case class FtanElement(name: Option[String],  attributes: Map[String, FtanValue]) extends FtanValue with SizedObject with TextComponent {
  import FtanElement._

  if (attributes.values.exists {x => x == FtanNull}) throw new IllegalArgumentException("Element cannot contain attribute with null value");

  /**
   * Create an element with no name, no attributes, and no content
   */
  def this() = this(None, Map[String, FtanValue]())

  /**
   * Create an element with a specified name, with no attributes and no content
   */

  def this(name: String) = this(Some(name), Map[String, FtanValue]())

  /**
   * Create an element with no name, and with attributes supplied in the form of a map
   */

  def this(attributes: Map[String,FtanValue]) = this(None, attributes)

  /**
   * Create an element with a name, and with attributes supplied in the form of a map
   */

  def this(name: String, attributes: Map[String,FtanValue]) = this(Some(name), attributes)

  /**
   * Create a new element as a copy of an existing element with a specified name
   */

  def setName(name: String) = new FtanElement(Some(name), this.attributes)

  /**
   * Create a new element as a copy of an existing element with an additional or replaced attribute
   */

  def setAttribute(name: String, value: FtanValue) = {
    if (value == FtanNull) this else new FtanElement(this.name, attributes + (name -> value))
  }

  /**
   * Create a new element as a copy of an existing element with an additional or replaced attributes
   */

  def setAttributes(attributes: Map[String,  FtanValue]) = new FtanElement(this.name, this.attributes ++ attributes.filter(_._2 != FtanNull))

  /**
   * Create a new element as a copy of an existing element with specified content
   */

  def setContent(value: FtanValue) = setAttribute(CONTENT_KEY, value)

  /**
   * Get a named attribute of the element (allows the form element("attname"))
   */

  def apply(attName: String) : FtanValue = attributes.getOrElse(attName, FtanNull)

  /**
   * Get the content of the element
   */

  def content: FtanValue = apply(CONTENT_KEY)

  def isNullContent: Boolean = content == FtanNull


  /**
   * Send the element to an Acceptor
   */

  override def send(acceptor: Acceptor) {
    acceptor.processStartElement(name)
    for ((key, value) <- attributes) {
      if (key != CONTENT_KEY && value != FtanNull) {
        acceptor.processAttributeName(key)
        value.send(acceptor)
      }
    }
    if (content != FtanNull) {
      acceptor.processAttributeName(CONTENT_KEY)
      content.send(acceptor)
    }
    acceptor.processEndElement()
  }

  override def equals(that: Any) = {
//    Console.println("name OK? " + (name == that.asInstanceOf[FtanElement].name) +
//      "attsize OK? " + (attributes.size == that.asInstanceOf[FtanElement].attributes.size) +
//      "attVal OK? " + attributes.equals(that.asInstanceOf[FtanElement].attributes) )
    that.isInstanceOf[FtanElement] &&
      name == that.asInstanceOf[FtanElement].name &&
      attributes.equals(that.asInstanceOf[FtanElement].attributes)
}

  override def hashCode() : Int = {
    attributes.hashCode()
  }

  /**
   * Get the size of the element, defined as the number of attributes excluding the name and content
   */

  def size = {
  	var size = attributes.size
  	
  	//don't want to count name and content as attributes
  	//if(attributes.contains(NAME_KEY))
  	//	size -= 1
  	if(attributes.contains(CONTENT_KEY))
  		size -= 1

  	size
  }
}