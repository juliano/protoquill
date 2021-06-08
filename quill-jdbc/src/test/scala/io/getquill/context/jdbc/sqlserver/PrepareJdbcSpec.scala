package io.getquill.context.jdbc.sqlserver

import java.sql.ResultSet
import io.getquill._

import io.getquill.context.jdbc.PrepareJdbcSpecBase
import org.scalatest.BeforeAndAfter

class PrepareJdbcSpec extends PrepareJdbcSpecBase with BeforeAndAfter {

  val context: testContext.type = testContext
  import testContext._

  before {
    testContext.run(query[Product].delete)
  }

  val prepareQuery = prepare(query[Product])
  // change to implicit val im: InsertMeta[Product] ... and interesting "Must be Uprootable or Pluckable" error will appear
  inline given InsertMeta[Product] = insertMeta[Product](_.id)

  "single" in {
    val prepareInsert = prepare(query[Product].insert(lift(productEntries.head)))

    singleInsert(dataSource.getConnection)(prepareInsert) mustEqual false
    extractProducts(dataSource.getConnection)(prepareQuery) === List(productEntries.head)
  }

  "batch" in {
    val prepareBatchInsert = prepare(
      liftQuery(withOrderedIds(productEntries)).foreach(p => query[Product].insert(p))
    )

    batchInsert(dataSource.getConnection)(prepareBatchInsert).distinct mustEqual List(false)
    extractProducts(dataSource.getConnection)(prepareQuery) === withOrderedIds(productEntries)
  }
}
