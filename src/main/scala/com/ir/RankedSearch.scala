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

  def get_idf(term: String): Float = {
    val map_values = invertedIndex.valuesIterator.toList.flatten
    val docs = mutable.HashSet[Int]()

    for (entry <- map_values) {
      docs.add(entry(0))
    }

    val N = docs.size
    val n_i = invertedIndex(term).size

    math.log(N/n_i).toFloat
  }

  def get_cos(query: List[String]): Int = {

    var query_vector: List[Float] = Nil

    for (key <- invertedIndex.keySet) {
      if (query.contains(key)) {
        query_vector :+= get_idf(key)
      }
    }

    println()
    print("query vector of ");println(query)
    println(query_vector)
    println()

    val query_docs = mutable.HashSet[Int]()

    //computing query docs
    for (term <- query) {
      for (arr <- invertedIndex(term)) {//TODO exception handling!
        query_docs.add(arr(0))
      }
    }
    println(query_docs)

    //create doc vectors
    val doc_vectorList = Nil
    var doc_vector: List[Float] = Nil

    for (doc <- query_docs) {
      doc_vector = Nil
      for (term <- invertedIndex.keySet) {
        val tf = invertedIndex(term).filter(arr => arr(0).equals(doc))(0)(1)
        doc_vector :+= (tf*get_idf(term))
      }
      println(doc_vector)
    }
    doc_vectorList ::: doc_vector
    //cosine calculation
    0
  }
}


object RankedSearch {

  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {

    val r = new QueryProcessor

    //3.1
    r.read("reuters-21578-index-snowball.txt")
    println(s"Number of terms: ${r.num_of_types}")
    print(s"Posting list of 'hillard': ")
    print_array(r.get_postingList("hillard"))
    println()


    //3.2
    println(s"IDF-Score of 'sugar': ${r.get_idf("sugar")}")
    println(r.get_cos("sugar soviet".split(" ").toList))
    println()

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