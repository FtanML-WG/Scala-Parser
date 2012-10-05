package types

import org.scalatest.FlatSpec
import ftanml.objects._
import ftanml.types.NameType
import util.TypeTest

class NameTest extends FlatSpec with TypeTest{

	"Named elements" should "be matched correctly by NameType" in {
		"<''>" ==> new NameType(FtanString(""), false)
		"<name att='test'>" ==> new NameType(FtanString("name"), false)
		"<'\u0074est\"'>" ==> new NameType(FtanString("test\""), false)
		"<name>" ==> new NameType(FtanString("[a-z]+"), true)
		"<Name>" !=> new NameType(FtanString("[a-z]+"), true)
		"<name2>" !=> new NameType(FtanString("[a-z]+"), true)
	}
	
	"Unnamed elements" should "not be matched by NameType" in {
		//TODO: Should they? Perhaps they should be matched by NameType(FtanString(""))
		"<>" !=> new NameType(FtanString(""), false)
		"<attr='test'>" !=> new NameType(FtanString("attr"), false)
		"<>" !=> new NameType(FtanString(".*"), true)
	}
	
	"Other values" should "not be matched by NameType" in {
		"'name'" !=> new NameType(FtanString("name"), false)
		"['name']" !=> new NameType(FtanString("name"), false)
		"0" !=> new NameType(FtanString("0"), false)
		"null" !=> new NameType(FtanString("null"), false)
		"false" !=> new NameType(FtanString("[a-z]+"), true)
	}
}