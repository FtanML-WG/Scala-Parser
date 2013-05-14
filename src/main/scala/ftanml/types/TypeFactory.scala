package ftanml.types

import ftanml.objects._
import ftanml.util.Implicits._
import ftanml.types.TypeFactory.InvalidTypeException
import ftanml.grammar._


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
    "list" -> ((e: FtanElement) => {if (e.isNullContent) ListType else listWithGrammar(e.content)}),
    "element" -> ((e: FtanElement) => {checkEmpty(e); ElementType}),
    "text" -> ((e: FtanElement) => {checkEmpty(e); TextType}),
    "value" -> ((e: FtanElement) => {checkEmpty(e); AnyType}),
    "nothing" -> ((e: FtanElement) => {checkEmpty(e); NothingType}),
    "nullable" -> ((e: FtanElement) => {new AnyOfType(List(NullType, makeType(singletonChildElement(e))))}),
    "not" -> ((e: FtanElement) => {new ComplementType(makeType(singletonChildElement(e)))}),
    "anyOf" -> ((e: FtanElement) => {new AnyOfType(contentTypes(e))}),
    "allOf" -> ((e: FtanElement) => {new AllOfType(contentTypes(e))})
  )
  
  val facets = collection.immutable.HashMap[String, (FtanType, FtanValue => FtanType)] (
  	"attName" -> (ElementType, (v: FtanValue) => new AttNameType(makeType(v))),
    "eq" -> (AnyType, (v: FtanValue) => new FixedValueType(v)),
    "enum" -> (ListType, (v: FtanValue) => new EnumerationType(v.values)),
    "form" -> (ElementType, (v: FtanValue) => elementProforma(v)),
    "itemType" -> (ElementType, (v: FtanValue) => new ItemType(makeType(v))),
    "ge" -> (NumberType, (v: FtanValue) => new MinValueType(v, false)),
    "grammar" -> (ElementType, (v: FtanValue) => listWithGrammar(v)),
    "gt" -> (NumberType, (v: FtanValue) => new MinValueType(v, true)),
    "le" -> (NumberType, (v: FtanValue) => new MaxValueType(v, false)),
    "lt" -> (NumberType, (v: FtanValue) => new MaxValueType(v, true)),
    "name" -> (ElementType, (v: FtanValue) => new NameType(makeType(v))),
    //"nameMatches"-> (StringType, (v: FtanValue) => new NameType(v, true)),
    "pattern"-> (StringType, (v: FtanValue) => new RegexType(v)),
    "size"-> (NumberType, (v: FtanValue) => {
      v match {
        case n: FtanNumber => new SizeType(n.toInt)
        case _ => throw new IllegalArgumentException("size facet must be a number")
      }}),
    "step" -> (NumberType, (v: FtanValue) => {
      v match {
        case n: FtanNumber => new StepType(n.value)
        case _ => throw new IllegalArgumentException("step facet must be a number")
      }})

  )

  def checkEmpty(e: FtanElement) {
    if (!e.isNullContent) {
      throw new InvalidTypeException("Type descriptor <" + e.name + "> must be empty")
    }
  }

  def contentTypes(e: FtanElement) : Traversable[FtanType] = {
    e.content match {
      case c: FtanList =>
        c.values.map {
          _ match {
            case el: FtanElement => makeType(el)
            case _ => throw new InvalidTypeException("Type descriptor <" + e.name + "> must have element-only content")
          }
        }
    }
  }

  def singletonChildElement(e: FtanElement) : FtanElement = {
    e.content match {
       case c: FtanElement => c
       case _ => throw new InvalidTypeException("Type descriptor <" + e.name + "> must have a single element as its content")
    }
  }

  def elementProforma(element: FtanElement) : FtanType = {
    val memberTypes : Traversable[FtanType] = {
      val namedType: Option[NameType] =
        element.name match {
          case Some(n) => Some(new NameType(new FixedValueType(FtanString(n))))
          case None => None
        }

      val extraTypes: Iterable[FtanType] =
        element.attributes.map {
          case (name, value) =>
            if (!value.isInstanceOf[FtanElement]) {
              throw new InvalidTypeException("Invalid value for " +
                (name match {
                  case "" => "content"
                  case _ => "attribute" + name
                }) + " in element proforma: expected a type")
            }
            new AttributeType(name, makeType(value))

        }

      namedType match {
        case Some(t) => t :: extraTypes.toList
        case None => extraTypes.toList
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

  def listWithGrammar(top: FtanElement) : Grammar = {
    var (min, max) = getOccurs(top)
    top.name match {
      case Some("seq") =>
        new SequenceParticle(min, max, particles(top.content)).makeGrammar
      case Some("choice") =>
        new ChoiceParticle(min, max, particles(top.content)).makeGrammar
      case Some(typeName) =>
        new LeafParticle(min, max, makeType(top)).makeGrammar
      case None =>
        throw new InvalidTypeException("Unknown type name <" + top.name + ">")
    }

  }

  def getOccurs(top: FtanElement) : (Int,  Int) = {
    var occurs = top("occurs")
    occurs match {
      case FtanNull => (1, 1)
      case a: FtanList => (a(0).asInstanceOf[FtanNumber].toInt,
        a(1) match {
          case FtanNull => ftanml.grammar.Particle.UNBOUNDED;
          case _ => a(1).asInstanceOf[FtanNumber].toInt
        })
      case _ => throw new InvalidTypeException("Invalid occurs value <" + occurs + ">")
    }
  }

  private def particles(content: FtanList) : Seq[ftanml.grammar.Particle] = {
    content.values.map((v: FtanValue) => {
      v match {
        case e: FtanElement => {
          e.name match {
            case Some("seq") =>
              val min = e("min") match {
                case n : FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              val max = e("max") match {
                case n : FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              new SequenceParticle(min, max, particles(e.content))
            case Some("many") =>
              new SequenceParticle(0, -1, particles(e.content))
            case Some("optional") =>
              new SequenceParticle(0, 1, particles(e.content))
            case Some("anyOf") =>
              val min = e("min") match {
                case n: FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              val max = e("max") match {
                case n: FtanNumber => n.value.asInstanceOf[Int]
                case _ => 1
              }
              new ChoiceParticle(min, max, particles(e.content))
            case _ =>
              new LeafParticle(1, 1, makeType(e))
          }
        }
        case _ => new ChoiceParticle(1, 1, particles(FtanList()))// TODO: error
      }
    })

  }

  def makeType(element: FtanElement): FtanType = {
    val memberTypes: List[FtanType] = {
      val namedType: Option[FtanType] =
        element.name match {
          case Some(n) =>
            namedTypes.get(n) match {
              case Some(t) => Some(t(element))
              case _ => throw new InvalidTypeException("Unknown type name <" + n + ">")
            }
          case None => None
        }
      val facetTypes: Iterable[FtanType] =
        element.attributes.map {case (name, value) =>
          facets.get(name) match {
            case Some((t: FtanType, f: (FtanValue => FtanType))) => {
              if(!value.isInstance(t)){
                throw new InvalidTypeException("Invalid value for " + name + " facet")
              }
              f(value).asInstanceOf[FtanType]
            }
            case _ => AnyType  // ignore unrecognized facets
          }
        }
      namedType match {
        case Some(t) => t :: facetTypes.toList
        case None => facetTypes.toList
      }
    }

    if(memberTypes.size == 0) {
      AnyType
    } else if (memberTypes.size == 1) {
      memberTypes.head.asInstanceOf[FtanType]
    } else {
      new AllOfType(memberTypes.asInstanceOf[Traversable[FtanType]])
    }
  }

}
