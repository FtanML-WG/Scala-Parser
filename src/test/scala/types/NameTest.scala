package types

import org.scalatest.FlatSpec
import ftanml.objects._
import util.TypeTest
import ftanml.types._

class NameTest extends FlatSpec with TypeTest{

  def fixedNameType(s : String) = new NameType(new FixedValueType(FtanString(s)))
  def namePatternType(s : String) = new NameType(new RegexType(FtanString(s)))

	"Named elements" should "be matched correctly by NameType" in {
		"<''>" ==> fixedNameType("")
		"<name att='test'>" ==> fixedNameType("name")
		"<'\u0074est\"'>" ==> fixedNameType("test\"")
		"<name>" ==> namePatternType("[a-z]+")
		"<Name>" !=> namePatternType("[a-z]+")
		"<name2>" !=> namePatternType("[a-z]+")
	}
	
	"Unnamed elements" should "not be matched by non-nullable NameType" in {
		"<>" !=> fixedNameType("")
		"<attr='test'>" !=> fixedNameType("attr")
		"<>" !=> namePatternType(".*")
    "<|blue>" ==> new NameType(new AnyOfType(List(NullType, StringType)))
	}
	
	"Other values" should "not be matched by NameType" in {
		"'name'" !=> fixedNameType("name")
		"['name']" !=> fixedNameType("name")
		"0" !=> fixedNameType("0")
		"null" !=> fixedNameType("null")
		"false" !=> namePatternType("[a-z]+")
	}
}