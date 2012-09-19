package ftanml.objects

import java.io.Writer

import scala.collection.mutable.LinkedHashMap

object FtanElement {
  val NAME_KEY = new FtanString("name")
  val CONTENT_KEY = new FtanString("content")
}

case class FtanElement(attributes: LinkedHashMap[FtanString, FtanValue]) extends FtanValue {
  import FtanElement._

  override def toFtanML(writer: Writer) {
    var space_needed = false

    // Opening bracket
    writer.append("<");

    // Write name, if existing
    attributes.get(NAME_KEY) map {
      case string: FtanString =>
        string.toFtanMLName(writer)
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
        key.toFtanMLName(writer)
        writer.append("=")
        value.toFtanML(writer)
        space_needed = true
    }

    //If there is valid content, write it
    attributes.get(CONTENT_KEY) map {
      case content: FtanArray if content.isValidElementContent =>
        writer.append("|")
        content.toFtanMLContent(writer)
      case _ =>
    }

    // Closing bracket
    writer.append(">");
  }

  def toFtanMLContent(writer: Writer) = toFtanML(writer)
}