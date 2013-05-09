package ftanml.objects


object FtanText extends FtanText(Nil) {
  //There is at least one argument
  def apply(first: TextComponent, rest: TextComponent*) = new FtanText(first::rest.toList)

  //zero argument case - empty content
  def apply() = new FtanText(List())

  //single string as content
  def apply(s: String) = if (s.isEmpty) new FtanText(List()) else new FtanText(List(FtanString(s)))

  //single element as content
  def apply(e: FtanElement) = new FtanText(List(e))
}

case class FtanText(values: Seq[TextComponent]) extends FtanValue with SizedObject {

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
    acceptor.processStartText()
    values.foreach {
      _.asInstanceOf[FtanValue].send(acceptor)
    }
    acceptor.processEndText()
  }

  lazy val size = values.length
}