package ftanml.grammar

import ftanml.types.FtanType

object Particle {
  def UNBOUNDED = -1
}


class Particle (min: Int, max: Int) {



  def makeGrammar: Grammar = {
    val machine = new Machine();
    val endState = machine.allocateState(true)
    val initialState = compileParticle(machine, endState)
    machine.resolveLambdaTransitions()
    //machine.print()
    new Grammar(initialState)
  }

  def compileParticle(machine: Machine, endState: State) : State = {
    var n : State = endState
    if (max == Particle.UNBOUNDED) {
      val t = machine.allocateState(false)
      val b = compileTerm (machine, t)
      b.addLambdaTransition (n)
      t.addLambdaTransition (b)
      n = b
    } else if (max > min) {
      for (i <- 1 to (max - min)) {
        val b = compileTerm (machine, n)
        b.addLambdaTransition (endState)
        n = b
      }
    }
    for (i <- 1 to min) {
      n = compileTerm (machine, n)
    }
    n
  }

  def compileTerm(machine: Machine, endState: State) : State =
    this match {
      case SequenceParticle(_, _, body) =>
        var n: State = endState
        for (p <- body.reverse) {
          n = p.compileParticle(machine, n)
        }
        n
      case ChoiceParticle(_, _, body) =>
        val b: State = machine.allocateState(false)
        for (p <- body) {
          val c = p.compileParticle(machine, endState)
          b.addLambdaTransition(c)
        }
        b
      case LeafParticle(_, _, theType) =>
        val b: State = machine.allocateState(false)
        val edge = new Edge(theType, endState)
        b.addTransition(edge)
        b
      case _ =>
        endState
    }

}

case class SequenceParticle(min : Int,  max: Int, body : Seq[Particle]) extends Particle(min, max)

case class ChoiceParticle(min : Int,  max: Int, body : Seq[Particle]) extends Particle(min, max)

case class LeafParticle(min : Int,  max: Int, theType : FtanType) extends Particle(min, max)

object LeafParticle {
  def apply(theType: FtanType) = new LeafParticle(1, 1, theType)
}

