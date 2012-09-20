package ftanml.objects

import java.io.Writer

import scala.collection.mutable.LinkedHashMap

object FtanElement extends FtanElement(new LinkedHashMap[FtanString,FtanValue]) {
  val NAME_KEY = new FtanString("name")
  val CONTENT_KEY = new FtanString("content")
  
  def apply(attributes: (FtanString,FtanValue)*) = new FtanElement(attributes.toMap)
}

case class FtanElement(attributes: LinkedHashMap[FtanString, FtanValue]) extends FtanValue {
  import FtanElement._
  
  def this(attributes: Map[FtanString,FtanValue]) = this(new LinkedHashMap++=attributes)

  override def writeFtanML(writer: Writer) {
    var space_needed = false

    // Opening bracket
    writer.append("<");

    // Write name, if existing
    attributes.get(NAME_KEY) map {
      case string: FtanString =>
        string.writeFtanMLName(writer)
        space_needed = true
      case _ =>
    }

    //Write all attributes (except name and content attribute)
    for ((key, value) <- attributes) value match {
      //Ignore name attribute, if valid
      case string: FtanString if key == NAME_KEY =>
      //Ignore content attribute, if valid
      case array: FtanArray if key == CONTENT_KEY && array.isValidElementContent =>
      //Write all other attributes
      case value: FtanValue =>
        if (space_needed)
          writer.append(" ")
        key.writeFtanMLName(writer)
        writer.append("=")
        value.writeFtanML(writer)
        space_needed = true
    }

    //If there is valid content, write it
    attributes.get(CONTENT_KEY) map {
      case content: FtanArray if content.isValidElementContent =>
        writer.append("|")
        content.writeFtanMLContent(writer)
      case _ =>
    }

    // Closing bracket
    writer.append(">");
  }

  def writeFtanMLContent(writer: Writer) = writeFtanML(writer)

  override def writeJson(writer: Writer) {
    def writeAttribute(attr:(FtanString,FtanValue)) {
      attr._1.writeJson(writer)
      writer.append(":")
      attr._2.writeJson(writer)
    }
    
    // Opening bracket
    writer.append("{");

    //Write all attributes
    if (attributes.size >= 1) {
      writeAttribute(attributes.head)
      for (element <- attributes.tail) {
        writer.append(",")
        writeAttribute(element)
      }
    }

    // Closing bracket
    writer.append("}");
  }
}