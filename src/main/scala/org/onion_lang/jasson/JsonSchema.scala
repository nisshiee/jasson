package org.onion_lang.jasson

/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:45
 */
object JsonSchema {
  sealed trait SchemaEntry
  case class CoreSchema(description: Option[String], typ: Option[String], properties: Map[String, SchemaEntry]) extends SchemaEntry
  // TODO required AST nodes should be added later
}