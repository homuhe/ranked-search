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
class QueryProcessor extends InvertedIndex {

  private val invertedIndex = inverted

  def get_idf(term: String): Double = {
    val map_values = invertedIndex.valuesIterator.toList.flatten
    val docs = mutable.HashSet[Int]()

    for (entry <- map_values) {
      docs.add(entry(0))
    }

    val N = docs.size
    val n_i = invertedIndex(term).size

    math.log(N/n_i)
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
    //val reuters = new InvertedIndex

    //reuters.read("reuters-21578-index-snowball.txt")
    //println(reuters.num_of_types)
    //print(reuters.get_postingList("hillard"))




    //use QueryProcessor same as InvertedIndex Class (as it is extended)
    val r = new QueryProcessor

    //3.1
    r.read("reuters-21578-index-snowball.txt")
    println(s"Number of terms: ${r.num_of_types}")
    print(s"Posting list of 'hillard': ")
    print_array(r.get_postingList("hillard"))

    //3.2
    println(s"IDF-Score of 'sugar': ${r.get_idf("sugar")}")

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
  def print_array(result: List[Array[Int]]) = {
    result.foreach(entry => print(entry.deep.mkString(" ") + " "))
    println
  }

}
