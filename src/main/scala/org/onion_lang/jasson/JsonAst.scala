package org.onion_lang.jasson

/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:31
 */
object JsonAst {
  sealed trait JsonValue
  case class JsonObject(kvs: List[(String, JsonValue)]) extends JsonValue
  object JsonObject {
    def apply(kvs: (String, JsonValue)*): JsonObject = JsonObject(kv.toList)
  }
  case class JsonArray(values : List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue

  /*
   * In JSON specification, integer and real are not separated.  However, I introduce them
   * for convenience.
   */

  case class JsonInteger(value: Int) extends JsonValue
  case class JsonReal(value: Double) extends JsonValue
}