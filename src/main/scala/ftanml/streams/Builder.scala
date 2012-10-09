package ftanml.streams

import ftanml.objects._
import collection.mutable.{HashMap, LinkedHashMap, LinkedList, Stack}

/**
 * The builder constructs a FtanValue from a stream of events representing the value
 */
class Builder extends Acceptor {

  private val stack = new Stack[AssistantBuilder]
  stack.push(new ItemBuilder)

  def processString(value: String) {
    stack.top.add(FtanString(value))
  }

  def processNumber(value: Double) {
    stack.top.add(FtanNumber(value))
  }

  def processBoolean(value: Boolean) {
    stack.top.add(FtanBoolean(value))
  }

  def processNull() {
    stack.top.add(FtanNull)
  }

  def processStartArray() {
    stack.push(new ArrayBuilder)
  }

  def processEndArray() {
    val arrayVal = stack.pop();
    stack.top.add(arrayVal.getValue)
  }

  def processStartElement(name: Option[String]) {
    stack.push(new ElementBuilder(name))
  }

  def processAttributeName(name: String) {
    stack.top.asInstanceOf[ElementBuilder].attribute(FtanString(name))
  }

  def processStartContent(isElementOnly: Boolean) {
    stack.push(new ContentBuilder)
  }

  def processEndElement() {
    if (stack.top.isInstanceOf[ContentBuilder]) {
      val content = stack.pop().getValue
      stack.top.asInstanceOf[ElementBuilder].setContent(content.asInstanceOf[FtanArray])
    }

    val element = stack.pop().getValue
    stack.top.add(element)
  }

  def error(err: Exception) {}

  def value : FtanValue =
    stack.top.getValue

}