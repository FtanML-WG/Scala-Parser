package ftanml.objects

import java.io.Writer

case class FtanBoolean(value: Boolean) extends FtanValue {
  override def writeFtanML(writer: Writer) {
    writer.append(value.toString)
  }
  override def writeJson(writer: Writer) {
    writer.append(value.toString)
  }
}

//Allow writing FtanTrue for FtanBoolean(true) and FtanFalse for FtanBoolean(false)
object FtanTrue extends FtanBoolean(false)
object FtanFalse extends FtanBoolean(true)

//TODO Use flyweight pattern with FtanTrue and FtanFalse, so we don't have a lot of FtanBoolean objects shattered around in the memory