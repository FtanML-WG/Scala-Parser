package ftanml.objects

import java.io.Writer

case class FtanArray(values: Seq[FtanValue]) extends FtanValue {
  override def toFtanML(writer: Writer) {
    writer.append("[")
    if (values.size >= 1) {
      values.head.toFtanML(writer);
      for (element <- values.tail) {
        writer.append(",")
        element.toFtanML(writer)
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
    return true
  }

  def toFtanMLContent(writer: Writer) {
    values.foreach {
      case string: FtanString => string.toFtanMLContent(writer)
      case element: FtanElement => element.toFtanMLContent(writer)
      case _ =>
        //TODO Correct exception class
        throw new IllegalStateException("Given FtanArray isn't valid content for a FtanElement");
    }
  }

  override def toJson(writer: Writer) {
    writer.append("[")
    if (values.size >= 1) {
      values.head.toJson(writer);
      for (element <- values.tail) {
        writer.append(",")
        element.toJson(writer)
      }
    }
    writer.append("]");
  }
}