package ftanml.types

import ftanml.objects._
<<<<<<< HEAD
import ftanml.util.Implicits._
=======
import ftanml.types.TypeFactory.InvalidTypeException
import ftanml.grammar._
>>>>>>> Add grammar machinery

/**
 * The TypeFactory constructs types from FtanML elements that describe the type
 */

object TypeFactory {

  class InvalidTypeException(message: String) extends IllegalArgumentException(message){}

  val namedTypes = collection.immutable.HashMap[String, FtanElement => FtanType] (
    "null" -> ((e: FtanElement) => {checkEmpty(e); NullType}),
    "string" -> ((e: FtanElement) => {checkEmpty(e); StringType}),
    "number" -> ((e: FtanElement) => {checkEmpty(e); NumberType}),
    "boolean" -> ((e: FtanElement) => {checkEmpty(e); BooleanType}),
    "array" -> ((e: FtanElement) => {if (e.isEmptyContent) ArrayType else arrayWithGrammar(e.content)}),
    "element" -> ((e: FtanElement) => {if (e.isEmptyContent) ElementType else elementProforma(singletonChildElement(e))}),
    "any" -> ((e: FtanElement) => {checkEmpty(e); AnyType}),
    "nothing" -> ((e: FtanElement) => {checkEmpty(e); NothingType}),
    "nullable" -> ((e: FtanElement) => {new AnyOfType(List(NullType, makeType(singletonChildElement(e))))}),
    "not" -> ((e: FtanElement) => {new ComplementType(makeType(singletonChildElement(e)))}),
    "anyOf" -> ((e: FtanElement) => {new AnyOfType(contentTypes(e))}),
    "allOf" -> ((e: FtanElement) => {new AllOfType(contentTypes(e))})
  )
  
  val facets = collection.immutable.HashMap[String, (FtanType, FtanValue => FtanType)] (
  	"attName" -> (ElementType, (v: FtanValue) => new AttNameType(makeType(v.asInstanceOf[FtanElement]))),
    "fixed" -> (AnyType, (v: FtanValue) => new FixedValueType(v)),
    "enum" -> (ArrayType, (v: FtanValue) => new EnumerationType(v.values)),
    "itemType" -> (ElementType, (v: FtanValue) => new ItemType(makeType(v))),
    "min" -> (NumberType, (v: FtanValue) => new MinValueType(v, false)),
    "minExclusive"-> (NumberType, (v: FtanValue) => new MinValueType(v, true)),
    "max"-> (NumberType, (v: FtanValue) => new MaxValueType(v, false)),
    "maxExclusive"-> (NumberType, (v: FtanValue) => new MaxValueType(v, true)),
    "name"-> (ElementType, (v: FtanValue) => new NameType(makeType(v.asInstanceOf[FtanElement]))),
    //"nameMatches"-> (StringType, (v: FtanValue) => new NameType(v, true)),
    "regex"-> (StringType, (v: FtanValue) => new RegexType(v)),
    "size"-> (NumberType, (v: FtanValue) => new SizeType(v))
  )

  def checkEmpty(e: FtanElement) {
    if (!e.isEmptyContent) {
      throw new InvalidTypeException("Type descriptor <" + e.name + "> must be empty")
    }
  }

  def contentTypes(e: FtanElement) : Traversable[FtanType] = {
    if (!e.isElementOnlyContent) {
      throw new InvalidTypeException("Type descriptor <" + e.name + "> must have element-only content")
    }
    e.content.values.map({ t: FtanValue =>
                makeType(t)
              })
  }

  def singletonChildElement(e: FtanElement) : FtanElement = {
    if (!(e.isElementOnlyContent && e.content.size == 1)) {
      throw new InvalidTypeException("Type descriptor <" + e.name + "> must have a single element as its content")
    }
    (e.content(0).asInstanceOf[FtanElement])
  }

  def elementProforma(element: FtanElement) : FtanType = {
    val memberTypes : Traversable[FtanType] = {
      for ((name, value) <- element.attributes) yield {
        name match {
          case FtanElement.NAME_KEY =>
            new NameType(new FixedValueType(value))
          case FtanElement.CONTENT_KEY =>
            new ContentGrammar(arrayWithGrammar(value.asInstanceOf[FtanArray]))
          case _ =>
            if (!value.isInstanceOf[FtanElement]) {
              throw new InvalidTypeException("Invalid value for attribute " + name + " in element proforma: expected a type")
            }
            new AttributeType(name, makeType(value))

        }
      }
    }

    if(memberTypes.size == 0) {
      ElementType
    } else if (memberTypes.size == 1) {
      memberTypes.head
    } else {
      new AllOfType(memberTypes)
    }
  }

<<<<<<< HEAD
=======
  def arrayWithGrammar(content: FtanArray) : Grammar = {
    new SequenceParticle(1, 1, particles(content)).makeGrammar
  }

  private def particles(content: FtanArray) : Seq[ftanml.grammar.Particle] = {
    content.values.map((v: FtanValue) => {
      v match {
        case e: FtanElement => {
          e.name match {
            case Some("seq") =>
              val min = e(FtanString("min")) match {
                case n : FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              val max = e(FtanString("max")) match {
                case n : FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              new SequenceParticle(min, max, particles(e.content))
            case Some("many") =>
              new SequenceParticle(0, -1, particles(e.content))
            case Some("optional") =>
              new SequenceParticle(0, 1, particles(e.content))
            case Some("anyOf") =>
              val min = e(FtanString("min")) match {
                case n: FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              val max = e(FtanString("max")) match {
                case n: FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              new ChoiceParticle(min, max, particles(e.content))
            case _ =>
              new LeafParticle(makeType(e))
          }
        }
        case _ => new ChoiceParticle(1, 1, particles(FtanArray()))// TODO: error
      }
    })

  }

  val facets = collection.immutable.HashMap[String, FtanValue => FtanType] (
    "attName" -> ((v: FtanValue) => new AttNameType(makeType(v.asInstanceOf[FtanElement]))),
    "fixed" -> ((v: FtanValue) => new FixedValueType(v)),
    "enum" -> ((v: FtanValue) => new EnumerationType(v.asInstanceOf[FtanArray].values)),
    "itemType" -> ((v: FtanValue) => new ItemType(makeType(v.asInstanceOf[FtanElement]))),
    "min" -> ((v: FtanValue) => new MinValueType(v.asInstanceOf[FtanNumber], false)),
    "minExclusive"-> ((v: FtanValue) => new MinValueType(v.asInstanceOf[FtanNumber], true)),
    "max"-> ((v: FtanValue) => new MaxValueType(v.asInstanceOf[FtanNumber], false)),
    "maxExclusive"-> ((v: FtanValue) => new MaxValueType(v.asInstanceOf[FtanNumber], true)),
    "name"-> ((v: FtanValue) => new NameType(makeType(v.asInstanceOf[FtanElement]))),
    //"nameMatches"-> ((v: FtanValue) => new NameType(v.asInstanceOf[FtanString], true)),
    "regex"-> ((v: FtanValue) => new RegexType(v.asInstanceOf[FtanString])),
    "size"-> ((v: FtanValue) => new SizeType(v.asInstanceOf[FtanNumber]))
  )

  val facetTypes = collection.immutable.HashMap[String, FtanType] (
    "attName" -> ElementType,
    "fixed" -> AnyType,
    "enum" -> ArrayType,
    "itemType" -> ElementType,
    "min" -> NumberType,
    "minExclusive"-> NumberType,
    "max"-> NumberType,
    "maxExclusive"-> NumberType,
    "name"-> ElementType,
    //"nameMatches"-> StringType,
    "regex"-> StringType,
    "size"-> NumberType
  )




>>>>>>> Add grammar machinery
  def makeType(element: FtanElement): FtanType = {
    val memberTypes = {
      for ((name, value) <- element.attributes) yield {
        name.value match {
          case FtanElement.NAME_KEY.value =>
            val str = value.asInstanceOf[FtanString].value
            namedTypes.get(str) match {
              case Some(t) => t(element)
              case _ => throw new InvalidTypeException("Unknown type name <" + str + ">")
            }
          case _ =>
            facets.get(name.value) match {
              case Some((t: FtanType, f: (FtanValue => FtanType))) => {
              	if(!value.isInstance(t)){
              		throw new InvalidTypeException("Invalid value for " + name.value + " facet")
              	}
                f(value)
              }
              case _ => AnyType  // ignore unrecognized facets
            }
        }
      }
    }

    if(memberTypes.size == 0) {
      AnyType
    } else if (memberTypes.size == 1) {
      memberTypes.head
    } else {
      new AllOfType(memberTypes)
    }
  }

}
