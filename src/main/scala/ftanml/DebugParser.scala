package ftanml

import com.sun.corba.se.spi.orbutil.fsm.Input
import scala.util.parsing.combinator.Parsers

trait DebugParser {
  self: Parsers =>
  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      println("testing for " + name + ".apply for token " + first + " at position " + pos + " offset " + offset)
      try {
        val t = parser.apply(in)
        println(name + ".apply for token " + first +
          " at position " + pos + " offset " + offset + " returns " + t)
        t
      } catch {
        case e: Exception =>
          println(name + ".apply threw exception: " + e.getMessage)
          throw e
        case b =>
          println(name + ".apply threw unknown exception "+b.toString)
          throw b
      }
    }
  }

  implicit def toWrapped(name: String) = new {
    def !!![T](p: Parser[T]) = new Wrap(name, p)
  }
}