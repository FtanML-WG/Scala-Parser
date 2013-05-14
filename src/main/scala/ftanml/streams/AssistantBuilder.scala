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
 * A ListBuilder is placed at the top of stack when an array is started, and is removed
 * when the array is closed.
 */

class ListBuilder() extends AssistantBuilder {
  var list = new ListBuffer[FtanValue]
  def add(value : FtanValue) {
    list.append(value)
  }
  def getValue = FtanList(list)
}

/**
 * A TextBuilder is an ArrayBuilder used for construction of mixed text content;
 * it exists as a separate class only so that it can be recognized during endElement
 * processing
 */

class TextBuilder extends AssistantBuilder {
  var list = new ListBuffer[TextComponent]
  def add(value : FtanValue) {
    value match {
      case t :TextComponent => list.append(t)
      case _ => throw new IllegalArgumentException("Wrong type of value for FtanML text")
    }
  }
  def getValue = FtanText(list)
}

/**
 * An ElementBuilder is placed on the top of stack when an elementStart event
 * occurs, and is removed when the corresponding endElement occurs.
 */

class ElementBuilder(name : Option[String]) extends AssistantBuilder {

  val map = new LinkedHashMap[String, FtanValue]
  var currentAtt : String = ""


  def attribute(name : String) {
    currentAtt = name
  }

  def add(value : FtanValue) {
    map.put(currentAtt, value)
  }

  def getValue = FtanElement(name, map.toMap)
}







