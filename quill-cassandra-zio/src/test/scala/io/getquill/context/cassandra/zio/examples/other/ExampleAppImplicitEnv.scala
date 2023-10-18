package io.getquill.context.cassandra.zio.examples.other

import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill._
import zio.Console.printLine
import zio.{ZIO, ZIOAppDefault}

object ExampleAppImplicitEnv extends ZIOAppDefault {

  object Ctx extends CassandraZioContext(Literal)

  case class Person(name: String, age: Int)

  inline def zioSessionLayer =
    CassandraZioSession.fromPrefix("testStreamDB")

  case class MyQueryService(cs: CassandraZioSession) {
    import Ctx.*
    implicit val env: Implicit[CassandraZioSession] = Implicit(cs)

    def joes   = Ctx.run(query[Person].filter(p => p.name == "Joe")).implicitly
    def jills  = Ctx.run(query[Person].filter(p => p.name == "Jill")).implicitly
    def alexes = Ctx.run(query[Person].filter(p => p.name == "Alex")).implicitly
  }

  override def run = {
    val result =
      for {
        csession <- ZIO.scoped(zioSessionLayer.build)
        joes     <- MyQueryService(csession.get).joes
      } yield joes

    result
      .tap(result => printLine(result.toString))
      .exitCode
  }
}
