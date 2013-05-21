package ftanml.exec

import ftanml.objects.FtanValue

/**
 * Created by IntelliJ IDEA.
 * User: mike
 * Date: 20/05/2013
 * Time: 10:39
 * To change this template use File | Settings | File Templates.
 */

object BuiltInGlobals {
  def map = Map(
    "abs" -> ftanml.functions.abs,
    "contains" -> ftanml.functions.contains,
    "content" -> ftanml.functions.content,
    "endsWith" -> ftanml.functions.endsWith,
    "name" -> ftanml.functions.name,
    "startsWith" -> ftanml.functions.startsWith,
    "to" -> ftanml.functions.to
  )
}