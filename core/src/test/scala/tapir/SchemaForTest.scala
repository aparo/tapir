package tapir

import java.time.{LocalDate, LocalDateTime, OffsetDateTime}

import org.scalatest.{FlatSpec, Matchers}
import tapir.Schema._

class SchemaForTest extends FlatSpec with Matchers {
  it should "find schema for simple types" in {
    implicitly[SchemaFor[String]].schema shouldBe SString
    implicitly[SchemaFor[String]].isOptional shouldBe false

    implicitly[SchemaFor[Short]].schema shouldBe SInteger
    implicitly[SchemaFor[Int]].schema shouldBe SInteger
    implicitly[SchemaFor[Long]].schema shouldBe SInteger
    implicitly[SchemaFor[Float]].schema shouldBe SNumber
    implicitly[SchemaFor[Double]].schema shouldBe SNumber
    implicitly[SchemaFor[Boolean]].schema shouldBe SBoolean
  }

  it should "find schema for java.time" in {
    implicitly[SchemaFor[LocalDate]].schema shouldBe SString
    implicitly[SchemaFor[LocalDate]].format shouldBe Some("date")
    implicitly[SchemaFor[LocalDateTime]].schema shouldBe SString
    implicitly[SchemaFor[LocalDateTime]].format shouldBe Some("date-time")
    implicitly[SchemaFor[OffsetDateTime]].schema shouldBe SString
    implicitly[SchemaFor[OffsetDateTime]].format shouldBe Some("date-time")
    implicitly[SchemaFor[OffsetDateTime]].formatOptions shouldBe Some("offset")
  }

  it should "find schema for optional types" in {
    implicitly[SchemaFor[Option[String]]].schema shouldBe SString
    implicitly[SchemaFor[Option[String]]].isOptional shouldBe true
  }

  it should "find schema for collections" in {
    implicitly[SchemaFor[Array[String]]].schema shouldBe SArray(SString)
    implicitly[SchemaFor[Array[String]]].isOptional shouldBe false

    implicitly[SchemaFor[List[String]]].schema shouldBe SArray(SString)
    implicitly[SchemaFor[List[String]]].isOptional shouldBe false

    implicitly[SchemaFor[Set[String]]].schema shouldBe SArray(SString)
  }

  val expectedASchema = SObject(SObjectInfo("A", "tapir.A"), List(("f1", SString), ("f2", SInteger), ("f3", SString)), List("f1", "f2"))

  it should "find schema for collections of case classes" in {
    implicitly[SchemaFor[List[A]]].schema shouldBe SArray(expectedASchema)
  }

  it should "find schema for a simple case class" in {
    implicitly[SchemaFor[A]].schema shouldBe expectedASchema
  }

  it should "find schema for a nested case class" in {
    implicitly[SchemaFor[B]].schema shouldBe SObject(SObjectInfo("B", "tapir.B"),
                                                     List(("g1", SString), ("g2", expectedASchema)),
                                                     List("g1", "g2"))
  }

  it should "find schema for case classes with collections" in {
    implicitly[SchemaFor[C]].schema shouldBe SObject(SObjectInfo("C", "tapir.C"),
                                                     List(("h1", SArray(SString)), ("h2", SInteger)),
                                                     List("h1"))
  }

  it should "find schema for trait" in {
    implicitly[SchemaFor[Animal]].schema shouldBe SObject(
      SObjectInfo("Animal", "tapir.Animal"),
      oneOf = List(
        SObject(SObjectInfo("Dog", "tapir.Dog"), List(("age", SInteger)), List("age")),
        SObject(SObjectInfo("Cat", "tapir.Cat"), List(("age", SInteger), ("leftLives", SInteger)), List("age", "leftLives"))
      )
    )
  }

}

case class A(f1: String, f2: Int, f3: Option[String])
case class B(g1: String, g2: A)
case class C(h1: List[String], h2: Option[Int])

sealed trait Animal
case class Dog(age: Int) extends Animal
case class Cat(age: Int, leftLives: Int) extends Animal
