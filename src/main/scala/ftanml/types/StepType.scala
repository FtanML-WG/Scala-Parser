package ftanml.types

import ftanml.objects.{FtanNumber, FtanElement, FtanValue}


/**
 * A type that only matches numbers that are integer multiples of some increment
 */

class StepType(increment : java.math.BigDecimal) extends FtanType {
  
  def matches(value: FtanValue) = value.isInstance(NumberType) &&
    value.asInstanceOf[FtanNumber].value.remainder(increment).compareTo(java.math.BigDecimal.ZERO) == 0

  def descriptor = new FtanElement().setAttribute("step", FtanNumber(increment))
}

object IntegerType extends StepType(java.math.BigDecimal.valueOf(1))