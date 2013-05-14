package ftanml.objects

import ftanml.streams.Acceptor

object FtanBoolean extends FtanBoolean(false)

case class FtanBoolean(value: Boolean) extends FtanValue {

  override def send(acceptor: Acceptor) {
    acceptor.processBoolean(value)
  }
}

//Allow writing FtanTrue for FtanBoolean(true) and FtanFalse for FtanBoolean(false)
object FtanTrue extends FtanBoolean(true)
object FtanFalse extends FtanBoolean(false)

//TODO Use flyweight pattern with FtanTrue and FtanFalse, so we don't have a lot of FtanBoolean objects shattered around in the memory