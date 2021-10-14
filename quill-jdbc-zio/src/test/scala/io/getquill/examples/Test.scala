package io.getquill.examples

import caliban.CalibanError.ExecutionError
import caliban.GraphQL.graphQL
import caliban.introspection.adt.{ __InputValue, __Type, __TypeKind }
import caliban.schema.{ ArgBuilder, Schema, Step }
import caliban.{ InputValue, RootResolver }

object Test extends App {

  case class ProductArgs[T](keyValues: Map[String, String])

  case class Name(first: String, last: String)
  case class Person(name: Name, age: Int)

  case class Queries(people: ProductArgs[Person] => List[Person])

  def flattenToPairs(key: String, value: InputValue): List[(String, String)] =
    value match {
      // If it contains other keys, continue to get the pairs inside
      // e.g. for `name` in Person(name: Name, age: Int) this would be Name from which we need first:String, last:String
      case InputValue.ObjectValue(fields) => fields.toList.flatMap { case (k, v) => flattenToPairs(k, v) }
      case _                              => List((key, value.toInputString))
    }

  implicit def productArgBuilder[T]: ArgBuilder[ProductArgs[T]] = {
    case InputValue.ObjectValue(fields) =>
      Right(ProductArgs[T](fields.flatMap { case (k, v) => flattenToPairs(k, v).toMap }))
    case other                          => Left(ExecutionError(s"Can't build a ProductArgs from input $other"))
  }

  implicit def productSchema[T](implicit ev: Schema[Any, T]): Schema[Any, ProductArgs[T]] =
    new Schema[Any, ProductArgs[T]] {

      def makeOptionalRecurse(f: __InputValue): __InputValue = {
        val fieldType         = f.`type`()
        val optionalFieldType = fieldType.kind match {
          case __TypeKind.NON_NULL => fieldType.ofType.getOrElse(fieldType)
          case _                   => fieldType
        }
        f.copy(`type` =
          () => optionalFieldType.copy(inputFields = optionalFieldType.inputFields.map(_.map(makeOptionalRecurse)))
        )
      }

      protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        __Type(
          __TypeKind.INPUT_OBJECT,
          inputFields = ev
            .toType_(isInput, isSubscription)
            .inputFields
            .map(_.map(f => makeOptionalRecurse(f)))
        )

      def resolve(value: ProductArgs[T]): Step[Any] = Step.NullStep
    }

  val api = graphQL(RootResolver(Queries { args =>
    println(args)
    Nil
  }))

  println(api.render)

  val query = """
  {
    people(name: { first: "n" }, age: 2) {
      name {
        first
      }
    }
  }"""

  val io =
    for {
      interpreter <- api.interpreter
      result      <- interpreter.execute(query)
      _            = println(result)
    } yield ()

  zio.Runtime.default.unsafeRun(io)
}
