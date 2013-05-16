package ftanml.exec

import ftanml.objects.{FtanNull, FtanList, FtanValue}


/**
 * Execution context for evaluating FtanSkrit expressions
 */
class Context(caller: Option[Context], globals: Map[String, FtanValue], args: Seq[FtanValue]) {

  var locals: Map[String, FtanValue] = Map()

  def evaluateVar(name: String): FtanValue = {
    locals.get(name) match {
      case Some(v) => v
      case None => {
        globals.get(name) match {
          case Some(g) => g
          case None => throw new IllegalArgumentException("Undeclared variable " + name)
        }
      }
    }
  }

  def evaluateParam(id: Int): FtanValue = {
    if (id == 0) {
      FtanList(args)
    } else if (id < 0 || id > args.size) {
      FtanNull
    } else {
      args(id - 1)
    }
  }

  def newContext(args: Seq[FtanValue]): Context = {
    new Context(Some(this), globals, args)
  }

}