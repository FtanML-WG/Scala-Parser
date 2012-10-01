package ftanml.streams

import ftanml.objects._
import collection.mutable.{HashMap, LinkedHashMap, LinkedList, Stack}

/**
 * The builder constructs a FtanValue from a stream of events representing the value
 */
class Builder extends Acceptor {

  private case class Info(var lastAtt : String, var map : LinkedHashMap[FtanString, FtanValue], var values : Seq[FtanValue])

  private val stack = new Stack[Info]

  def processString(value: String) {
    add(FtanString(value))
  }

  def processNumber(value: Double) {
    add(FtanNumber(value))
  }

  def processBoolean(value: Boolean) {
    add(FtanBoolean(value))
  }

  def processNull() {
    add(FtanNull)
  }

  def processStartArray() {
    stack.push(Info(null, new LinkedHashMap[FtanString, FtanValue], new LinkedList[FtanValue]))
  }

  def processEndArray() {
    val info = stack.pop();
    val array = FtanArray(info.values)
    add(array)
  }

  def processStartElement(name: Option[String]) {
    stack.push(Info(null, new LinkedHashMap[FtanString, FtanValue], new LinkedList[FtanValue]))
  }

  def processAttributeName(name: String) {
    stack.top.lastAtt = name
  }

  def processStartContent(isElementOnly: Boolean) {
    processAttributeName(FtanElement.CONTENT_KEY.value)
  }

  def processEndElement() {
    val info = stack.pop()
    val element = FtanElement(info.map)
    add(element)
  }

  def error(err: Exception) {}

  def add(value : FtanValue) {
    if (stack.isEmpty) {
      stack.push(Info(null, new LinkedHashMap[FtanString, FtanValue], new LinkedList[FtanValue]))
      stack.top.values = stack.top.values :+ value
    } else {
      stack.top.values = stack.top.values :+ value
      stack.top.map.put(FtanString(stack.top.lastAtt), value)
    }
  }

  def value : FtanValue =
    stack.top.values(0)

}