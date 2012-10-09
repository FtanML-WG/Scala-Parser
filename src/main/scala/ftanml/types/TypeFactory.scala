package ftanml.types

import ftanml.objects._
import ftanml.types.TypeFactory.InvalidTypeException

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
    "array" -> ((e: FtanElement) => {checkEmpty(e); ArrayType}),
    "element" -> ((e: FtanElement) => {if (e.isEmptyContent) ElementType else elementProforma(singletonChildElement(e))}),
    "any" -> ((e: FtanElement) => {checkEmpty(e); AnyType}),
    "nothing" -> ((e: FtanElement) => {checkEmpty(e); NothingType}),
    "nullable" -> ((e: FtanElement) => {new AnyOfType(List(NullType, makeType(singletonChildElement(e))))}),
    "not" -> ((e: FtanElement) => {new ComplementType(makeType(singletonChildElement(e)))}),
    "anyOf" -> ((e: FtanElement) => {new AnyOfType(contentTypes(e))}),
    "allOf" -> ((e: FtanElement) => {new AllOfType(contentTypes(e))})
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
                makeType(t.asInstanceOf[FtanElement])
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
            NullType // TODO (ignore for now)
          case _ =>
            if (!value.isInstanceOf[FtanElement]) {
              throw new InvalidTypeException("Invalid value for attribute " + name + " in element proforma: expected a type")
            }
            new AttributeType(name, makeType(value.asInstanceOf[FtanElement]))

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




  def makeType(element: FtanElement): FtanType = {
    val memberTypes = {
      for ((name, value) <- element.attributes) yield {
        name.value match {
          case FtanElement.NAME_KEY.value =>
            val str = value.asInstanceOf[FtanString].value
            namedTypes.get(str) match {
              case Some(t) => t.asInstanceOf[FtanElement => FtanType](element)
              case _ => throw new InvalidTypeException("Unknown type name <" + str + ">")
            }
          case _ =>
            facetTypes.get(name.value) match {
              case Some(t) =>
                if (!value.isInstance(t)) {
                  throw new InvalidTypeException("Invalid value for " + name.value + " facet")
                }
              case _ =>
            }
            facets.get(name.value) match {
              case Some(t) => {
                t.asInstanceOf[FtanValue => FtanType](value)
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