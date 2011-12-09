package org.onion_lang.jasson

/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:45
 */
object JsonSchema {
  sealed trait SchemaEntry
  sealed trait TypeSchema
  final case class ObjectSchema(name: String, description: Option[String], properties: List[(String, SchemaEntry)]) extends SchemaEntry
  final case class StringSchema(required: Boolean, minLength: Option[Int], maxLength: Option[Int]) extends SchemaEntry with TypeSchema
  final case class IntegerSchema(required: Boolean, maximum: Option[Int], minimum: Option[Int]) extends SchemaEntry with TypeSchema
  final case class NumberSchema(required: Boolean,  maximum: Option[Int], minimum: Option[Int]) extends SchemaEntry with TypeSchema
  final case class BooleanSchema(required: Boolean) extends SchemaEntry with TypeSchema
  final case class AnySchema(required: Boolean) extends SchemaEntry with TypeSchema
  final case class RefSchema(required: Boolean,  ref: String) extends SchemaEntry with TypeSchema
  final case class NullSchema(required: Boolean) extends SchemaEntry with TypeSchema
  final case class ArraySchema(required: Boolean,  baseType: TypeSchema,  minItems: Option[Int], maxItems: Option[Int]) extends SchemaEntry with TypeSchema
}