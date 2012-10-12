package ftanml.grammar

import ftanml.types.{ArrayType, FtanType}
import ftanml.objects.{FtanElement, FtanArray, FtanValue}


class Grammar(initialState: State) extends FtanType {
  def matches(value: FtanValue) = {
    value.isInstance(ArrayType) &&
    (Set(initialState) /: value.asInstanceOf[FtanArray].values) ((s: Set[State], v: FtanValue) => {
      s.map(_.makeTransition(v)).flatten
    }).exists(_.isFinal)
  }

  def descriptor = new FtanElement("grammar") // TODO
}