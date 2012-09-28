package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types.NameType
import util.TypeTest

class NameTest extends FlatSpec with TypeTest{

	"Named elements" should "be matched correctly by NameType" in {
		"<''>" ==> new NameType(FtanString(""))
		"<name att='test'>" ==> new NameType(FtanString("name"))
		"<'\u0074est\"'>" ==> new NameType(FtanString("test\""))
	}
	
	"Unnamed elements" should "not be matched by NameType" in {
		//TODO: Should they? Perhaps they should be matched by NameType(FtanString(""))
		"<>" !=> new NameType(FtanString(""))
		"<attr='test'>" !=> new NameType(FtanString("attr"))
	}
	
	"Other values" should "not be matched by NameType" in {
		"'name'" !=> new NameType(FtanString("name"))
		"['name']" !=> new NameType(FtanString("name"))
		"0" !=> new NameType(FtanString("0"))
		"null" !=> new NameType(FtanString("null"))
	}
}