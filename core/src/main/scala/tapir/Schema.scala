package tapir

import java.time.{LocalDate, LocalDateTime, OffsetDateTime}

import Schema._

import language.experimental.macros
import magnolia._

sealed trait Schema {
  def show: String
}

// TODO: configuration; camel-case/normal case
// TODO: test recurrence
object Schema {
  case object SString extends Schema {
    def show: String = "string"
  }
  case object SInteger extends Schema {
    def show: String = "integer"
  }
  case object SNumber extends Schema {
    def show: String = "number"
  }
  case object SBoolean extends Schema {
    def show: String = "boolean"
  }

//  case class SRef(id:String) extends Schema {
//    def show: String = "ref"
//  }

  case class SObject(info: SObjectInfo,
                     fields: Iterable[(String, Schema)] = None,
                     required: Iterable[String] = None,
                     oneOf: Iterable[Schema] = None)
      extends Schema {
    def show: String = s"object(${fields.map(f => s"${f._1}->${f._2.show}").mkString(",")};required:${required.mkString(",")})"
  }
  case class SArray(element: Schema) extends Schema {
    def show: String = s"array(${element.show})"
  }

  case class SObjectInfo(shortName: String, fullName: String)
}

trait SchemaFor[T] {
  def schema: Schema
  def isOptional: Boolean = false
  def format: Option[String] = None
  def formatOptions: Option[String] = None
  def show: String = s"schema is $schema"
}

object SchemaFor extends SchemaForMagnoliaDerivation {
  implicit case object SchemaForString extends SchemaFor[String] {
    override val schema: Schema = SString
  }
  implicit case object SchemaForShort extends SchemaFor[Short] {
    override val schema: Schema = SInteger
  }
  implicit case object SchemaForInt extends SchemaFor[Int] {
    override val schema: Schema = SInteger
  }
  implicit case object SchemaForLong extends SchemaFor[Long] {
    override val schema: Schema = SInteger
  }
  implicit case object SchemaForFloat extends SchemaFor[Float] {
    override val schema: Schema = SNumber
  }
  implicit case object SchemaForDouble extends SchemaFor[Double] {
    override val schema: Schema = SNumber
  }
  implicit case object SchemaForBoolean extends SchemaFor[Boolean] {
    override val schema: Schema = SBoolean
  }

  // Date time
  implicit case object SchemaForLocalDate extends SchemaFor[LocalDate] {
    override val schema: Schema = SString
    override val format: Option[String] = Some("date")
  }

  implicit case object SchemaForLocalDateTime extends SchemaFor[LocalDateTime] {
    override val schema: Schema = SString
    override val format: Option[String] = Some("date-time")
  }

  implicit case object SchemaForOffsetDateTime extends SchemaFor[OffsetDateTime] {
    override val schema: Schema = SString
    override val format: Option[String] = Some("date-time")

    override val formatOptions: Option[String] = Some("offset")
  }

  implicit def schemaForOption[T: SchemaFor]: SchemaFor[Option[T]] = new SchemaFor[Option[T]] {
    override def schema: Schema = implicitly[SchemaFor[T]].schema
    override def isOptional: Boolean = true
  }

  implicit def schemaForArray[T: SchemaFor]: SchemaFor[Array[T]] = new SchemaFor[Array[T]] {
    override def schema: Schema = SArray(implicitly[SchemaFor[T]].schema)
  }
  implicit def schemaForIterable[T: SchemaFor, C[_] <: Iterable[_]]: SchemaFor[C[T]] = new SchemaFor[C[T]] {
    override def schema: Schema = SArray(implicitly[SchemaFor[T]].schema)
  }
}

trait SchemaForMagnoliaDerivation {
  type Typeclass[T] = SchemaFor[T]

  def combine[T](ctx: CaseClass[SchemaFor, T]): SchemaFor[T] = {
    new SchemaFor[T] {
      override val schema: Schema = SObject(
        SObjectInfo(ctx.typeName.short, ctx.typeName.full),
        ctx.parameters.map(p => (p.label, p.typeclass.schema)).toList,
        ctx.parameters.filter(!_.typeclass.isOptional).map(_.label)
      )
    }
  }

  def dispatch[T](ctx: SealedTrait[SchemaFor, T]): SchemaFor[T] = {
    new SchemaFor[T] {
      override val schema: Schema = SObject(
        SObjectInfo(ctx.typeName.short, ctx.typeName.full),
        oneOf = ctx.subtypes.toList.map(_.typeclass.schema)
      )
    }

  }

  implicit def schemaForCaseClass[T]: SchemaFor[T] = macro Magnolia.gen[T]
}
