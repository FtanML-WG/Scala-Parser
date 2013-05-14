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

  def processNumber(value: java.math.BigDecimal) {
    stack.top.add(FtanNumber(value))
  }

  def processBoolean(value: Boolean) {
    stack.top.add(FtanBoolean(value))
  }

  def processNull() {
    stack.top.add(FtanNull)
  }

  def processStartList() {
    stack.push(new ListBuilder)
  }

  def processEndList() {
    val arrayVal = stack.pop();
    stack.top.add(arrayVal.getValue)
  }

  def processStartText() {
    stack.push(new TextBuilder())
  }

  def processEndText() {
    val arrayVal = stack.pop();
    stack.top.add(arrayVal.getValue)
  }

  def processStartElement(name: Option[String]) {
    stack.push(new ElementBuilder(name))
  }

  def processAttributeName(name: String) {
    stack.top.asInstanceOf[ElementBuilder].attribute(name)
  }

  def processEndElement() {
    val element = stack.pop().getValue
    stack.top.add(element)
  }

  def error(err: Exception) {}

  def value : FtanValue =
    stack.top.getValue

}