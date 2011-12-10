package org.onion_lang.jasson

import util.parsing.combinator.RegexParsers
import JsonSchema._

/**
 * Created by IntelliJ IDEA.
 * User: Mizushima
 * Date: 11/12/10
 * Time: 7:40
 * To change this template use File | Settings | File Templates.
 */

object JaconParser extends RegexParsers {
  lazy val K_OBJECT: Parser[String] = "object"
  lazy val K_IDENT: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  lazy val K_LBRACE: Parser[String] = "{"
  lazy val K_RBRACE: Parser[String] = "}"
  lazy val K_BEGIN_PREAMBLE: Parser[String] = "<<<<"
  lazy val K_END_PREAMBLE: Parser[String] = ">>>>"
  lazy val K_SEMICOLON: Parser[String] = ";"
  lazy val K_COLON: Parser[String] = ":"
  lazy val INTEGER: Parser[Int] = """[0-9_]*""".r ^^ {_.toInt}
  def parseFrom(input: CharSequence): ParseResult[SchemaUnit] = parseAll(all, input)
  lazy val all: Parser[SchemaUnit] = preamble ~ objectProperty ^^ { case pre ~ ((name, oschema)) => SchemaUnit(name, pre, oschema) }
  lazy val preamble: Parser[String] = K_BEGIN_PREAMBLE ~> """((?!(>>>>))(\[rnfb"'\\]|[^\\]))+""".r <~ K_END_PREAMBLE
  lazy val typeSchema: Parser[TypeSchema] = stringSchema | integerSchema | numberSchema | booleanSchema | anySchema | refSchema | nullSchema | arraySchema
  lazy val property: Parser[(String, SchemaEntry)] = objectProperty | typedProperty
  lazy val typedProperty: Parser[(String, TypeSchema)] = K_IDENT ~ (K_COLON ~> typeSchema) ^^ { case name ~ tpe => (name, tpe) }
  lazy val objectProperty: Parser[(String, ObjectSchema)] = (K_OBJECT ~> K_IDENT  <~ K_LBRACE) ~ (property.* <~ K_RBRACE)^^ { case name ~ props => (name, ObjectSchema(None, props)) }
  lazy val stringSchema: Parser[StringSchema] = "string" ~> (opt("?" ^^ {x => false}) ~ opt("min" ~> "=" ~> INTEGER) ~ opt("max" ~> "=" ~> INTEGER)) ^^ {
    case required ~ min ~ max  => StringSchema(required.getOrElse(true), min, max)
  }
  lazy val numberSchema: Parser[NumberSchema] = "number" ~> (opt("?" ^^ {x => false}) ~ opt("min" ~> "=" ~> INTEGER) ~ opt("max" ~> "=" ~> INTEGER)) ^^ {
    case required ~ min ~ max  => NumberSchema(required.getOrElse(true), min, max)
  }
  lazy val integerSchema: Parser[IntegerSchema] = "integer" ~> (opt("?" ^^ {x => false}) ~ opt("min" ~> "=" ~> INTEGER) ~ opt("max" ~> "=" ~> INTEGER)) ^^ {
    case required ~ min ~ max  => IntegerSchema(required.getOrElse(true), min, max)
  }
  lazy val booleanSchema: Parser[BooleanSchema] = "boolean" ~> opt("?" ^^ {x => false}) ^^ { case required => BooleanSchema(required.getOrElse(true)) }
  lazy val anySchema: Parser[AnySchema] = "any" ~> opt("?" ^^ {x => false}) ^^ { case required => AnySchema(required.getOrElse(true)) }
  lazy val refSchema: Parser[RefSchema] = "ref" ~> opt("?" ^^ {x => false}) ~ K_IDENT ^^ { case required ~ ref => RefSchema(required.getOrElse(true), ref) }
  lazy val nullSchema: Parser[NullSchema] = "null" ~> opt("?" ^^ {x => false}) ^^ { case required => NullSchema(required.getOrElse(true)) }
  lazy val arraySchema: Parser[ArraySchema] = "array" ~> (opt("?" ^^ {x => false}) ~ typeSchema ~ opt("minItems" ~> "=" ~> INTEGER) ~ opt("maxItems" ~> "=" ~> INTEGER)) ^^ {
    case required ~ baseType ~ minItems ~ maxItems  => ArraySchema(required.getOrElse(true), baseType, minItems, maxItems)
  }
}