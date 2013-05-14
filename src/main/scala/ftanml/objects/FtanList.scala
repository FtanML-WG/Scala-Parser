package ftanml.objects

import java.io.Writer
import ftanml.streams.Acceptor


object FtanList extends FtanList(Nil) {
  //There is at least one argument
  def apply(first: FtanValue, rest: FtanValue*) = new FtanList(first::rest.toList)
  //zero argument case
  def apply() = new FtanList(Nil)
}

case class FtanList(values: Seq[FtanValue]) extends FtanValue with SizedObject {

  /**
   * _(i) selects the i'th item of the FtanArray
   */
  def apply(i : Int) = values(i)

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


  override def send(acceptor: ftanml.streams.Acceptor) {
    acceptor.processStartList()
    values.foreach {
      _.send(acceptor)
    }
    acceptor.processEndList()
  }

//  def writeFtanMLContent(writer: Writer) {
//    values.foreach {
//      case string: FtanString => string.writeFtanMLContent(writer)
//      case element: FtanElement => element.writeFtanMLContent(writer)
//      case _ =>
//        //TODO Correct exception class
//        throw new IllegalStateException("Given FtanArray isn't valid content for a FtanElement");
//    }
//  }


  override def equals(obj: Any) = {
    obj.isInstanceOf[FtanList] &&
      size == obj.asInstanceOf[FtanList].size &&
      !(values, obj.asInstanceOf[FtanList].values).zipped.exists(_ != _)
  }

  lazy val size = values.length
}