package ftanml.objects

import java.io.Writer
import ftanml.streams.Acceptor
import ftanml.exec.Context


object FtanList extends FtanList(Nil) {
  //There is at least one argument
  def apply(first: FtanValue, rest: FtanValue*) = new FtanList(first::rest.toList)
  //zero argument case
  def apply() = new FtanList(Nil)
}

case class FtanList(values: Seq[FtanValue]) extends FtanValue with SizedObject {

  /**
   * _(i) selects the i'th item of the FtanList (zero-based)
   */
  def apply(i : Int): FtanValue = values(i)

  /**
   * Applies a function to each item of the list
   */

  def map(c: Context, f : FtanFunction): FtanList = {
    FtanList(values.map {(v:FtanValue) => f(c, List(v)).asInstanceOf[FtanValue]})
  }

  /**
   * Selects item from the list based on the value of a function
   */

  def filter(c: Context, f : FtanFunction): FtanList = {
    FtanList(values.filter {(v:FtanValue) => f(c, List(v)).asBoolean("result of filter expression")})
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