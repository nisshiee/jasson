package org.onion_lang.jasson

import scala.util.parsing.json._
/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:53
 */
trait CodeGenerator {
  val outputDir: String
  def generateCode(schema: JSONObject): Unit
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
    def generateCode(schema: JSONObject): Unit = {
      val obj = schema.obj
      val genClassName = obj("className")
      obj("type") match {
        case "string" =>
        case "number" =>
        case "integer" =>
        case "object" =>
          val className = obj("className")
          printf("class %d(mapping: Map[String, Any]) {%n", className)
          val properties = obj("properties").asInstanceOf[JSONObject].obj
          for((k, tpe:Map[String, Any]) <- properties) {
            val tpString = typeMapping(tpe("type").toString)
            printf("def %s: %s = mapping.get(\"%s\").asInstanceOf[%s]", k, tpString, k, tpString)
          }
          printf("}%n")
          for((k, tpe:JSONObject) <- properties) {
            val obj = tpe.obj
            val stype = typeMapping(obj("type").toString)
            if(stype == "object") {
              generateCode(tpe)
            }
          }
        case "array" =>
      }
    }
  }
  def apply(outputDir: String): CodeGenerator = new ScalaCodeGenerator(outputDir)
}
