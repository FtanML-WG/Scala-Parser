package ftanml.objects

import java.io.Writer

object FtanArray extends FtanArray(Nil) {
  //There is at least one argument
  def apply(first: FtanValue, rest: FtanValue*) = new FtanArray(first::rest.toList)
  //zero argument case
  def apply() = new FtanArray(Nil)
}

case class FtanArray(values: Seq[FtanValue]) extends FtanValue with GetSize {
  override def writeFtanML(writer: Writer) {
    writer.append("[")
    if (values.size >= 1) {
      values.head.writeFtanML(writer);
      for (element <- values.tail) {
        writer.append(",")
        element.writeFtanML(writer)
      }
    }
    writer.append("]");
  }

  /**
   * Check, if this FtanArray can be output in the content area of a tag
   *
   * @return True, if this FtanArray could be used in the content area of a tag
   */
  def isValidElementContent: Boolean = {
    values.foreach {
      case string: FtanString =>
      case element: FtanElement =>
      case _ => return false
    }
    true
  }

  def writeFtanMLContent(writer: Writer) {
    values.foreach {
      case string: FtanString => string.writeFtanMLContent(writer)
      case element: FtanElement => element.writeFtanMLContent(writer)
      case _ =>
        //TODO Correct exception class
        throw new IllegalStateException("Given FtanArray isn't valid content for a FtanElement");
    }
  }

  override def writeJson(writer: Writer) {
    writer.append("[")
    if (values.size >= 1) {
      values.head.writeJson(writer);
      for (element <- values.tail) {
        writer.append(",")
        element.writeJson(writer)
      }
    }
    writer.append("]");
  }
  
  def size = values.length
}