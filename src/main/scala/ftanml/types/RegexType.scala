package ftanml.types

import ftanml.objects.FtanString
import ftanml.objects.FtanValue


/**
 * A type that matches a value based on a regular expression. As in XSD, the regex is
 * implicitly anchored.
 *
 * Note: for the time being, we use Scala's regular expressions natively, rather than say XSD regular expressions.
 */

class RegexType(pattern : FtanString) extends FtanType {
  
  private val regex = ("^" + pattern.value + "$").r
  
  def matches(value: FtanValue) = {
    value match {
      case v : FtanString => regex.findFirstIn(v.value) != None
      case _ => false
    }
  }
}