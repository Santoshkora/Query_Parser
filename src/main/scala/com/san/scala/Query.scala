package com.san.scala


/**
  * Created by Yesu on 24-03-2016.
  */
case class Query(projections: List[(String, Option[String])], tables: List[(String, Option[String])], joins: List[(String, Option[String], Criteria)], criteria: Option[Criteria], groupBy: Option[String], orderBy: Option[String], limit: Option[Int], offset: Option[Int])
