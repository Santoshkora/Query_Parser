package com.san.scala.test

import com.san.scala.QueryParser
import org.scalatest.{Matchers, FunSpec}

/**
  * Created by Yesu on 28-03-2016.
  */
class QueryParserTest extends FunSpec with Matchers{
  describe("Query Parser") {

    val queryParser = new QueryParser

    it("should handle null properly") {
      an[IllegalArgumentException] should be thrownBy {
        queryParser.findColumns4(null)
      }
    }

    it("should return only the columns before the FROM keyword") {
      val query = "SELECT value1, val2, value3 FROM TableValue;"
      val words = query.split("\\s+").toList.filter(_.trim.length > 0)
      queryParser.findColumns4(words) shouldBe List("value1,","val2,", "value3")
    }

    it("should return only the columns before the AS keyword") {
      val query = "value1 AS VAL1, value2 AS VAL2"
      val words = query.split("\\s+").toList.filter(_.trim.length > 0)
      queryParser.findColumns3(words).distinct shouldBe List("value1","value2")
    }

    it("should return only the columns from this type as 'table_name.column_name'") {
      val query = "TableValue.value1, TableVallue.value2"
      val words = query.split("\\s+").toList.filter(_.trim.length > 0)
      queryParser.findColumns2(words,'.') shouldBe List("value1,", "value2")
    }

    it("should return only the columns after the FROM keyword") {
      val query = "SELECT value1, value3 FROM TableValue WHERE val2 = 'game'"
      val words = query.split("\\s+").toList.filter(_.trim.length > 0)
      queryParser.findColumns1(words) shouldBe List("val2")
    }


  }
}
