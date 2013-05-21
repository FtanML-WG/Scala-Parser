package functions

import org.scalatest.FlatSpec
import util.ParserTest
import ftanml.objects._

class FunctionTest extends ParserTest with FlatSpec {
  "OrFunctions" should "work" in {
    "{true || false}" ~~> FtanBoolean(true)
    "{true || true}" ~~> FtanBoolean(true)
    "{false || true}" ~~> FtanBoolean(true)
    "{false || false}" ~~> FtanBoolean(false)
  }

  "AndFunctions" should "work" in {
    "{true && false}" ~~> FtanBoolean(false)
    "{true && true}" ~~> FtanBoolean(true)
    "{false && true}" ~~> FtanBoolean(false)
    "{false && false}" ~~> FtanBoolean(false)
  }

  "Comparisons" should "work" in {
    "{3 = 3}" ~~> FtanBoolean(true)
    "{3 = 4}" ~~> FtanBoolean(false)
    "{false != true}" ~~> FtanBoolean(true)
    "{5 > 3}" ~~> FtanBoolean(true)
    "{5 >= 6}" ~~> FtanBoolean(false)
    "{5 <= 6}" ~~> FtanBoolean(true)
    "{5 != 6}" ~~> FtanBoolean(true)
    "{5 > 6}" ~~> FtanBoolean(false)
  }

  "Arithmetic" should "work" in {
    "{2+2}" ~~> FtanNumber(4)
    "{2 - 1}" ~~> FtanNumber(1)
    "{2 - 1 - 1}" ~~> FtanNumber(0)
    "{2×1.5 + 3}" ~~> FtanNumber(6)
    "{2×1.5 ÷ 3}" ~~> FtanNumber(1)
    "{2×1.5 + 3 = 6}" ~~> FtanTrue
    "{2×1.5 + 3 = 6 && 2 + 2 = 4}" ~~> FtanTrue
    "{8 ÷ 2 ÷ 2}" ~~> FtanNumber(2)
  }

  "Parentheses" should "work" in {
    "{(2)}" ~~> FtanNumber(2)
    "{(true) || (false)}" ~~> FtanTrue
    "{(2+1) × 3}" ~~> FtanNumber(9)
    "{3 × (2+1)}" ~~> FtanNumber(9)
    "{3-(2-1)}" ~~> FtanNumber(2)
    "{(2=3 || 4=2+2) && (5 <= 6)}" ~~> FtanTrue
  }

  "Subscripting" should "work" in {
    "{[1,2,3][0]}" ~~> FtanNumber(1)
    "{[1,2,3][1]}" ~~> FtanNumber(2)
    "{[[1,2,3][4,5,6]][1][1]}" ~~> FtanNumber(5)
  }

  "ClassicFunctionCalls" should "work" in {
    "{{1+1}()}" ~~> FtanNumber(2)
    "{{_+1}(4)}" ~~> FtanNumber(5)
    "{{_1+_2}(4, 5)}" ~~> FtanNumber(9)
    "{{_0}(4, 5, 6)}" ~~> FtanList(FtanNumber(4), FtanNumber(5), FtanNumber(6))
  }

  "ChainedFunctionCalls" should "work" in {
    "{1.{_+1}()}" ~~> FtanNumber(2)
    "{1.{_+1}().{_+1}()}" ~~> FtanNumber(3)
  }

  "ListsOfFunctions" should "work" in {
    "{[{1+_}, {2+_}][1](2)}" ~~> FtanNumber(4)
  }

  "FunctionsOfLists" should "work" in {
    "{{[1,2,3,4]}()[3]}" ~~> FtanNumber(4)
  }

  "Filtering" should "work" in {
    "{[1,2,3,4]?{_>2}}" ~~> FtanList(FtanNumber(3),FtanNumber(4))
    "{[<e id=1><e id=2><e id=3>]?{_@id>1}!{_@id}}" ~~> FtanList(FtanNumber(2), FtanNumber(3))
  }

  "AttributeExtraction" should "work" in {
    "{<x=2 y=3>@x}" ~~> FtanNumber(2)
    "{{<x=2 y=3>@(_)}(\"y\")}" ~~> FtanNumber(3)
    "{<x=2 y=<z=8>>@y@z}" ~~> FtanNumber(8)
    "{<x=2 y=[3,4,5]>@y[2]}" ~~> FtanNumber(5)
    "{(<x=2 y=[<z=8>]>@y[0])@z}" ~~> FtanNumber(8)
  }

  "LocalVariables" should "work" in {
    "{let x=3; let y=4; x+y}" ~~> FtanNumber(7)
    "{let inc={_+1}; let dec={_-1}; inc(inc(dec(3)))}" ~~> FtanNumber(4)
    "{let inc={_+1}; let dec={_-1}; 3.inc().inc().dec()}" ~~> FtanNumber(4)
    "{let add={_1+_2}; let sub={_1-_2}; 3.add(2).sub(4)}" ~~> FtanNumber(1)
  }

  "Ranges" should "work" in {
    "{1..3}" ~~> FtanList(FtanNumber(1), FtanNumber(2), FtanNumber(3))
    "{5.to(10)[4]}" ~~> FtanNumber(9)
    "{(1..3)!{_+_}}" ~~> FtanList(FtanNumber(2), FtanNumber(4), FtanNumber(6))
  }

  "BuiltInFunctions" should "work" in {
    "{<e>.name()}" ~~> FtanString("e")
    "{<>.name()}" ~~> FtanNull
    "{name(<e>)}"  ~~> FtanString("e")
    "{<e 2>.content()}" ~~> FtanNumber(2)
    "{<e>.content()}" ~~> FtanNull
    "{-3.14.abs()}" ~~> FtanNumber("3.14")
    "{contains(name(<abc>), name(<ab>))}" ~~> FtanTrue
    "{startsWith(name(<abc>), name(<ab>))}" ~~> FtanTrue
    "{endsWith(name(<abc>), name(<ab>))}" ~~> FtanFalse
  }

  "ConditionalExpressions" should "work" in {
    "{let x=2; if x<3 then <e> else <f>}" ~~> FtanElement("e")
  }

  "SyntaxErrors" should "be rejected, if wrong" in {
    "{3" invalid;
    "3}" invalid
  }

}