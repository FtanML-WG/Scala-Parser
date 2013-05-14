package ftanml.objects

import ftanml.streams.Acceptor

case class FtanNumber(value: java.math.BigDecimal) extends FtanValue {

  override def send(acceptor: Acceptor) {
    acceptor.processNumber(value)
  }

  def toInt = value.longValue().asInstanceOf[Int]

  override def equals(obj: Any) = obj.isInstanceOf[FtanNumber] && obj.asInstanceOf[FtanNumber].value.compareTo(value) == 0

  override def hashCode() = value.doubleValue().hashCode()
}

object FtanNumber extends FtanNumber(java.math.BigDecimal.ZERO) {
  def isWhole(value: java.math.BigDecimal) =
    value.scale == 0 || value.compareTo(value.setScale(0, java.math.BigDecimal.ROUND_DOWN)) == 0

  def apply(s: String) =
    new FtanNumber(new java.math.BigDecimal(s))

  def apply(d: Double) =
    new FtanNumber(new java.math.BigDecimal(d))

  def apply(l: Long) =
    new FtanNumber(new java.math.BigDecimal(l))

}