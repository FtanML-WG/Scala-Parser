package ftanml.grammar

import ftanml.types.{ListType, FtanType}
import ftanml.objects.{FtanElement, FtanList, FtanValue}


class Grammar(initialState: State) extends FtanType {
  def matches(value: FtanValue) = {
    value.isInstance(ListType) &&
    (Set(initialState) /: value.asInstanceOf[FtanList].values) ((s: Set[State], v: FtanValue) => {
      s.map(_.makeTransition(v)).flatten
    }).exists(_.isFinal)
  }

  def descriptor = new FtanElement("grammar") // TODO
}