package com.san

import scala.collection.mutable.{Map => MMap, ListBuffer}
import scala.util.control.Breaks._

/**
  * Created by Santu on 29-02-2016.
  * Its a sample program to extract certain elements from a multiline string.
  */
object SnowFlakes extends App {

  case class Criteria(operation: String, operand1: String, operand2: Option[String])

  case class Query(projections: List[(String, Option[String])], tables: List[(String, Option[String])], joins: List[(String, Option[String], Criteria)], criteria: Option[Criteria], groupBy: Option[String], orderBy: Option[String], limit: Option[Int], offset: Option[Int])

  val sampleelements ="""SELECT  name1 AS Name1, age AS AGE, film.film_id AS FID,  film.title AS title,
                         | film.description AS description,  category.name AS category,  film.rental_rate AS price,
                         | film.length AS length,  film.rating AS rating,  GROUP_CONCAT(CONCAT(actor.first_name, _utf8' ', actor.last_name) SEPARATOR ', ') AS actors
                         |FROM  category LEFT JOIN  film_category ON
                         |  category.category_id = film_category.category_id  LEFT JOIN film ON
                         |  film_category.film_id = film.film_id  JOIN film_actor ON
                         |  film.film_id = film_actor.film_id JOIN actor ON film_actor.actor_id = actor.actor_id  GROUP BY film.film_id;""".stripMargin.replaceAll(System.lineSeparator(), " ").trim
  val sampleelement = sampleelements.dropRight(1)
  val words = sampleelement.split("\\s+").toList.filter(_.trim.length > 0)

  def findNext(words: List[String], toFind: String): List[String] = {
    val list = ListBuffer[String]()
    words.zipWithIndex.foreach { pair =>
      if(pair._1.equalsIgnoreCase(toFind)) {
        if(words.size > pair._2 + 1) {
          list.append(words(pair._2 + 1))
        }
      }
    }
    list.toList
  }

  def findTablesAndColumns(words: List[String], toFind: Char): Map[String, List[String]]  = {
    val map = MMap[String, List[String]]()
    words.zipWithIndex.foreach { pair =>
      if(pair._1.contains(toFind)) {
        val tab: String = pair._1
        val table = before(tab,toFind.toString)
        val column = after(tab,toFind.toString)
        val list = map.getOrElseUpdate(table, List())
        map.put(table, list :+ column)
      }
      //if(pair._1.)
    }
    map.mapValues(_.distinct).toMap
  }

  //For columns as "c"
  def findColumns4(words: List[String]): List[String] = {
    val list = ListBuffer[String]()
    words.zipWithIndex.foreach { pair =>
      var indexTo = 0
      if(pair._1.equalsIgnoreCase("FROM")) {
        indexTo = pair._2
      }
      for (x <- 1 until indexTo) {
        list.append(words(x))
      }
    }
    list.toList
  }

  //For columns as "c AS alias_name"
  def findColumns3(words: List[String]): List[String] = {
    val list = ListBuffer[String]()
    val intermediatelist = ListBuffer[String]()
    words.zipWithIndex.foreach { pair =>
      for (x <- words){
        list.append(x)
        list.zipWithIndex.foreach { pair =>
          if(pair._1.equalsIgnoreCase("AS")) {
            list.remove(pair._2, pair._2 + 1)
            intermediatelist.append(list(pair._2 - 1))
          }C:\Users\Yesu\IdeaProjects\Sample1\src
        }
      }
    }
    intermediatelist.toList
  }

  //For columns as "t1.c"
  def findColumns2(words: List[String], toFind: Char): List[String] = {
    val list = ListBuffer[String]()
    words.zipWithIndex.foreach { pair =>
      if (pair._1.contains(toFind)) {
          val res1: String = pair._1
          val result = after(res1,toFind.toString)
          list.append(result)
      }else {
        if (pair._1.isEmpty == false) {
          list.append(words(pair._2))
        }
      }
    }
    list.toList
  }

  //For columns after the FROM Keyword in the query string.
  def findColumns1(words: List[String]): List[String] = {
    val list = ListBuffer[String]()
    var indexRange = 0
    words.zipWithIndex.foreach { pair =>
      var indexTo = 0
      if (pair._1.equalsIgnoreCase("FROM")) {
        indexTo = pair._2 + 1
        for (x <- indexTo until words.size) {
          list.append(words(x))
        }
      }
    }
    breakable {
      list.zipWithIndex.foreach { pair =>
        if (pair._1.equalsIgnoreCase("ON") || (pair._1.equalsIgnoreCase("WHERE"))) {
          indexRange = list.size - pair._2
          break()
        }
      }
    }
    val finallist = findColumnsFromOn(list.toList.takeRight(indexRange - 1))
    finallist
  }

  def findColumnsFromOn(words: List[String]) : List[String] = {
    val list = ListBuffer[String]()
    val checkelements = List("INNER","LEFT","RIGHT","FULL","CROSS","SELF","JOIN","WHERE","GROUP","HAVING","ORDER","LIMIT","OFFSET")
    words.zipWithIndex.foreach { pair =>
      for (x <- checkelements) {
        if (pair._1.equalsIgnoreCase(x.toString())) {
          val indexTo = pair._2
          val listelements = words.take(indexTo)
          for (elements <- listelements) {
            if (elements.contains('.')) {
              val res1: String = elements
              val result = after(res1, '.'.toString)
              list.append(result)
            }
          }
        }
      }
    }
    list.toList
  }

  def after(value: String,a: String): String = {
    // Returns a substring containing all characters after a string.
    val posA = value.lastIndexOf(a);
    if (posA.equals(-1)) {
      return "";
    }
    val adjustedPosA = posA + a.length();
    if (adjustedPosA >= value.length()) {
      return "";
    }
    return value.substring(adjustedPosA);
  }

  def before(value: String, a: String): String = {
    // Return substring containing all characters before a string.
    val posA = value.indexOf(a);
    if (posA.equals(-1)) {
      return "";
    }
    return value.substring(0, posA);
  }

  def between(value: String, a: String, b: String): String = {
    // Return a substring between the two strings.
    val posA = value.indexOf(a);
    if (posA.equals(-1)) {
      return "";
    }
    val posB = value.lastIndexOf(b);
    if (posB.equals(-1)) {
      return "";
    }
    val adjustedPosA = posA + a.length();
    if (adjustedPosA >= posB) {
      return "";
    }
    return value.substring(adjustedPosA, posB);
  }

  val f4 = findColumns4(words)
  val f3 = findColumns3(f4)
  val f2 = findColumns2(f3,'.').distinct
  val f1 = findColumns1(words).distinct
  val result = f2:::f1.distinct
  println(result)
  val res = findTablesAndColumns(words,'.')
  res.foreach(pair => {
    if (pair._1.nonEmpty){
      val tableName = pair._1
      println(s"COLUMNS for $tableName : " + res.get(s"$tableName"))
    }
  })
}