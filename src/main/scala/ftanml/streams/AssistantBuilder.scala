package ftanml.streams

import collection.mutable.{LinkedHashMap, ListBuffer}
import ftanml.objects._

/**
 * This is a collection of helper classes used by the Builder. The Builder maintains a stack of
 * AssistantBuilder objects, and always passes incoming strings, numbers, etc to the AssistantBuilder
 * that is currently on the top of the stack.
 */

trait AssistantBuilder {

  /**
   * Add a value to the composite value currently being assembled)
   */

  def add(value : FtanValue)

  /**
   * On completion get the assembled composite value
   */

  def getValue : FtanValue

}

/**
 * ItemBuilder is used only at the top level, in case the value being built is atomic
 */

class ItemBuilder() extends AssistantBuilder {
  var value : FtanValue = FtanNull
  def add(value : FtanValue) {
    this.value = value
  }
  def getValue = this.value
}

/**
 * An ArrayBuilder is placed at the top of stack when an array is started, and is removed
 * when the array is closed.
 */

class ArrayBuilder() extends AssistantBuilder {
  var list = new ListBuffer[FtanValue]
  def add(value : FtanValue) {
    list.append(value)
  }
  def getValue = FtanArray(list)
}

/**
 * A ContentBuilder is an ArrayBuilder used for construction of element content;
 * it exists as a separate class only so that it can be recognized during endElement
 * processing
 */

class ContentBuilder extends ArrayBuilder {}

/**
 * An ElementBuilder is placed on the top of stack when an elementStart event
 * occurs, and is removed when the corresponding endElement occurs.
 */

class ElementBuilder(name : Option[String]) extends AssistantBuilder {

  val map = new LinkedHashMap[FtanString, FtanValue]
  var currentAtt : FtanString = FtanString("")

  name match {
    case Some(s) => map.put(FtanElement.NAME_KEY, FtanString(s))
    case None =>
  }

  def attribute(name : FtanString) {
    currentAtt = name
  }

  def setContent(content : FtanArray) {
    map.put(FtanElement.CONTENT_KEY, content)
  }

  def add(value : FtanValue) {
    map.put(currentAtt, value)
  }

  def getValue = FtanElement(map)
}







