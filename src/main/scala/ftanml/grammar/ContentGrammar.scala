package ftanml.grammar

import ftanml.objects.{FtanElement, FtanArray, FtanValue}
import ftanml.types.{ElementType, ArrayType, FtanType}


class ContentGrammar(grammar: Grammar) extends FtanType {
  def matches(value: FtanValue) = {
    value.isInstance(ElementType) && grammar.matches(value.asInstanceOf[FtanElement].content)
  }

  def descriptor = new FtanElement("contentGrammar") // TODO
}