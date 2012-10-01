package ftanml.streams

import java.io.Writer
import collection.mutable.Stack
import ftanml.util.Unicode
import ftanml.objects.FtanElement

/**
 * This class is a filter which converts the special name and content attributes of an element
 * into standard attributes
 */

class ContentExpander(out : Acceptor) extends Filter(out) {

  val elementStack = new Stack[Boolean]

  override def processStartElement(name: Option[String]) {
    out.processStartElement(None)
    elementStack.push(false)
    name match {
      case Some(n) => {
        out.processAttributeName(FtanElement.NAME_KEY.value)
        out.processString(n)
      }
      case None =>
    }
  }

  override def processStartContent(isElementOnly: Boolean) {
    elementStack.pop()
    elementStack.push(true)
    out.processAttributeName(FtanElement.CONTENT_KEY.value)
    out.processStartArray()
  }

  override def processEndElement() {
    val inContent = elementStack.pop()
    if (inContent) {
      out.processEndArray()
    }
    out.processEndElement()
  }


}