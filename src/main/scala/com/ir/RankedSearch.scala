package com.ir

import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 3
  * Description:  Processes query of user input on inverted indices file.
  */

/**
  * Assignment 3.1
  * Class representing an inverted index with term frequencies
  */
class InvertedIndex {

  val inverted = mutable.HashMap[String, List[Array[Int]]]()

  def read(file: String): Unit = {
    val lines = Source.fromFile(file).getLines()

    for (line <- lines) {
      val term = line.split("\t")(0)
      val posting = line.split("\t")(1)
        .split("\\s+")
        .map(element => element.toInt)
        .sliding(2,2)
        .toList

      inverted += term -> posting
    }
  }

  def num_of_types: Int = {
    inverted.size
  }

  def get_postingList(term: String): List[Array[Int]] = {
    inverted(term)
  }
}

/**
  * Assignment 3.2
  * Class for calculating IDF, cosine similarity & TF-IDF
  */
class QueryProcessor {

  def get_idf(term: String): Int = {
    0
  }

  def get_cos(query: List[String], doc: List[String]): Int = {
   0
  }

  def tfidf_vector(query: List[String], doc: List[String]): List[Int] = {
    Nil
  }
}


object RankedSearch {

  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {

    println("Harambe!")
    val reuters = new InvertedIndex
    reuters.read("reuters-21578-index-snowball.txt")
    println(reuters.num_of_types)

    print(reuters.get_postingList("hillard"))

  }

  /**
    * Help function for correct usage
    */
  def help() = {
    println("Help function")
    sys.exit()
  }

  /**
    * print method for Option
   */
  def print(result: List[Array[Int]]) = {
      result.foreach(entry => println(entry.deep.mkString(" ")))
  }

}
