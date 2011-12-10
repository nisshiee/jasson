package org.onion_lang.jasson

import scala.util.parsing.json._
import JsonSchema._
/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:53
 */
trait CodeGenerator {
  val outputDir: String
  def generateCode(schema: JsonSchema.SchemaUnit): Unit
}

object CodeGenerator {
  private val typeMapping: Map[String, String] = Map(
    "string" -> "String",
    "number" -> "Double",
    "integer" -> "Int",
    "object" -> "Map[String, Any]",
    "array" -> "Array[Any]"
  )
  private class ScalaCodeGenerator(val outputDir: String) extends CodeGenerator {
    private var indent = 0
    def enter(beginFormat: String, end: String, args: String*)(block: => Any): Unit = {
      pn(beginFormat, args:_*)
      indent += 2
      block
      indent -= 2
      pn(end)
    }
    def pl(format: String,  args: String*): Unit = {
      print(" " * indent)
      printf(format, args:_*)
    }
    def nl(): Unit = {
      println()
    }
    def pn(format: String,  args: String*): Unit = {
      pl(format, args:_*)
      nl()
    }
    private def type2String(tpe: TypeSchema): String = tpe match {
      case _:StringSchema => "String"
      case _:IntegerSchema => "Int"
      case _:NumberSchema => "Double"
      case _:BooleanSchema => "Boolean"
      case _:AnySchema => "Any"
      case x:RefSchema => x.ref
      case _:NullSchema => "Null"
      case ArraySchema(_, base, _, _) =>
        "List[" + type2String(base) + "]"
    }
    private def genCodeFor(property: (String,  SchemaEntry)): Unit = {
      val (name, entry) = property
      entry match {
        case ObjectSchema(desc, properties) =>
          enter("class %s {", "}", name) {
            for(property <- properties) {
              genCodeFor(property)
            }
          }
          enter("object %s {", "}", name) {
            enter("def loadFrom(json: JSONObject): %s = {", "}", name) {
              pn("val newObject = new %s()", name)
              for((name, entry) <- properties) entry match {
                case StringSchema(req, min, max) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[String]", name, name)
                case IntegerSchema(req, min, max) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[Double].toInt", name, name)
                case NumberSchema(req, min, max) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[Double]", name, name)
                case BooleanSchema(req) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[Boolean]", name, name)
                case AnySchema(req) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[Any]", name, name)
                case RefSchema(req, ref) =>
                  pn("newObject.%s = %s.loadFrom(json.obj(\"%s\").asInstanceOf[JSONObject])", name, ref, name)
                case NullSchema(req) =>
                  pn("newObject.%s = json.obj(\"%s\").asInstanceOf[Null]", name, name)
                case ArraySchema(req, base, minItems, maxItems) =>
                  pn("val newList = json.obj(\"%s\").asInstanceOf[JSONArray].list", name)
                  enter("for(e <- newList) {", "}") {
                    pn("newObject.%s = e :: newObject.%s", name, name)
                  }
                  pn("newObject.%s = newObject.%s.reverse", name, name)
              }
              pn("newObject")
            }
            enter("def loadFrom(jsonStr: String): %s = {", "}", name) {
              pn("loadFrom(JSON.parseRaw(jsonStr).get.asInstanceOf[JSONObject])")
            }
          }
        case StringSchema(req, min, max) =>
          pn("var %s : %s = _", name, "String")
        case IntegerSchema(req, min, max) =>
          pn("var %s : %s = 0", name, "Int")
        case NumberSchema(req, min, max) =>
          pn("var %s : %s = 0.0d", name, "Double")
        case BooleanSchema(req) =>
          pn("var %s : %s = false", name, "Boolean")
        case AnySchema(req) =>
          pn("var %s : %s = _", name, "Any")
        case RefSchema(req, ref) =>
          pn("var %s : %s = null", name, ref)
        case NullSchema(req) =>
          pn("var %s : %s = null", name, "Null")
        case ArraySchema(req, base, minItems, maxItems) =>
          pn("var %s : %s = Nil", name, type2String(base))
      }
    }
    def generateCode(unit: SchemaUnit): Unit = {
      pn(unit.preamble)
      pn("import scala.util.parsing.json._")
      genCodeFor((unit.name,  unit.schema))
    }
  }
  def apply(outputDir: String): CodeGenerator = new ScalaCodeGenerator(outputDir)
}
