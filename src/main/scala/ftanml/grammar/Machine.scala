package ftanml.grammar

import collection.mutable.LinkedList
import ftanml.types.FtanType
import ftanml.objects.FtanValue


class Machine () {

  var states = new LinkedList[State]

  def allocateState(isFinal: Boolean) : State = {
    val numberOfStates = states.size
    val n = new State(numberOfStates, isFinal)
    states +:= n
    n
  }

  def resolveLambdaTransitions() {
    states.foreach(_.resolveLambdaTransitions())
  }

  def print() {
    println("FINITE STATE MACHINE")
    states.foreach(_.print())
  }
}


class State(val stateNumber: Int, var isFinal: Boolean) {

  var lambdaTransitions = new LinkedList[State]
  var transitions: List[Edge] = List()

  def addLambdaTransition (state: State) {
    lambdaTransitions +:= state
  }

  def addTransition (edge: Edge) {
    transitions +:= edge
  }

  def resolveLambdaTransitions() {
    if (!lambdaTransitions.isEmpty) {
      lambdaTransitions.foreach (_.resolveLambdaTransitions())
      isFinal |= lambdaTransitions.exists (_.isFinal)
      transitions = List.concat (transitions, (lambdaTransitions.map(_.transitions)).flatten)
      lambdaTransitions = new LinkedList[State];
    }
  }

  def makeTransition(value: FtanValue): Set[State] =
    collection.immutable.HashSet(transitions.filter(_.theType.matches(value)).map(_.endState) : _*)

  def print() {
    println("  " + stateNumber + (if (isFinal) " final" else ""))
    if (!lambdaTransitions.isEmpty) {
      println("    lambda transitions:")
      lambdaTransitions.foreach((e: State) => println("      -> " + e.stateNumber))
    }
    transitions.foreach((e: Edge) => println("      " + e.theType + " -> " + e.endState.stateNumber))
  }

}


case class Edge (theType: FtanType, endState: State)