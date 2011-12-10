package org.onion_lang.jasson

/*
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/10/29
 * Time: 9:45
 */
object JsonSchema {
  sealed trait SchemaEntry
  sealed trait TypeSchema extends SchemaEntry
  final case class SchemaUnit(name: String,  preamble: String,  schema: ObjectSchema)
  final case class ObjectSchema(description: Option[String], properties: List[(String, SchemaEntry)]) extends SchemaEntry
  final case class StringSchema(required: Boolean, minLength: Option[Int], maxLength: Option[Int]) extends TypeSchema
  final case class IntegerSchema(required: Boolean, minimum: Option[Int], maximum: Option[Int]) extends TypeSchema
  final case class NumberSchema(required: Boolean,  minimum: Option[Int], maximum: Option[Int]) extends TypeSchema
  final case class BooleanSchema(required: Boolean) extends TypeSchema
  final case class AnySchema(required: Boolean) extends TypeSchema
  final case class RefSchema(required: Boolean,  ref: String) extends TypeSchema
  final case class NullSchema(required: Boolean) extends TypeSchema
  final case class ArraySchema(required: Boolean,  baseType: TypeSchema,  minItems: Option[Int], maxItems: Option[Int]) extends TypeSchema
}