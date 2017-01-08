package com.ir

import scala.collection.{SortedMap, mutable}
import scala.io.{Source, StdIn}

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

  /**
    * Reads in inverted index file of format: term \t docID1 \s freq1 \s docID2 \s freq2
    * @param file inverted index
    */
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

  /**
    * Returns the number of terms in inverted index
    * @return number of terms
    */
  def num_of_terms: Int = {
    inverted.size
  }

  /**
    * Returns a posting list given its term
    * @param term of which a posting list is returned
    * @return posting list
    */
  def get_postingList(term: String): List[Array[Int]] = {
    if(inverted.contains(term))
      inverted(term)
    else Nil
  }

}

/**
  * Assignment 3.2
  * Class for calculating IDF, cosine similarity & TF-IDF scores
  */
class QueryProcessor extends InvertedIndex {

  private val invertedIndex = inverted
  private var doc_number = 0
  private val norm_scores = mutable.HashMap[Int, Double]()


  /**
    * Normalizes the sum of TF-IDFs of a given document
    *  which is used in the denominator of the cos sim calculation
    */
  def calculate_doc_norms(): Unit = {

    val doc_norms = mutable.HashMap[Int, List[Double]]()

    for((term, postinglist) <- invertedIndex) {
      for(posting <- postinglist) {

        val docID = posting(0)
        val tf = posting(1)

        //computing tf-idf & square of it
        val tf_idf_sq = (tf * get_idf(term)) * (tf * get_idf(term))

        if(!doc_norms.contains(docID))
          doc_norms.put(docID, List(tf_idf_sq))
        else doc_norms(docID) :+= tf_idf_sq
      }
    }

    var sum: Double = 0.0
    for((docID, tf_idf_sq) <- doc_norms) {

      for(value <- tf_idf_sq)
        sum += value

      sum = Math.sqrt(sum)

      norm_scores.put(docID, sum)
    }
  }

  /**
    * Calculates the number of docs in inverted index
    */
  def num_of_docs(): Unit = {
    val map_values = inverted.valuesIterator.toList.flatten
    val docs = mutable.HashSet[Int]()

    for (entry <- map_values)
      docs.add(entry(0))
    doc_number = docs.size
  }

  /**
    * Returns IDF score of a given term
    * @param term
    * @return IDF score
    */
  def get_idf(term: String): Double = {
    val N = doc_number
    val n_i = invertedIndex(term).size

    math.log(N/n_i)
  }

  /**
    * Returns the cosine similarities of a query & document pairs
    * @param query
    * @return Array of which each element consists of DocID & cosine similarity with query
    */
  def get_cos(query: List[String]): Array[(Int, Double)] = {

    var query_vector: List[Double] = Nil

    for(queryTerm <- query) {
      if(invertedIndex.keySet.contains(queryTerm))
        query_vector :+= 1 * get_idf(queryTerm)
    }


    val doc2Score = mutable.HashMap[Int, List[Double]]()

    //fetching posting lists for query term
    for(index <- query.indices) {

      val queryTerm = query(index)

      for(posting <- get_postingList(queryTerm)){
        val doc_id = posting(0)
        val tf = posting(1)
        val tf_idf = tf * get_idf(queryTerm)

        if(!doc2Score.contains(doc_id)) {
          val list: List[Double] = List.fill(query.length)(0)
          doc2Score(doc_id) = list.updated(index, tf_idf)
        }
        else doc2Score(doc_id).updated(index, tf_idf)
      }
    }

    //calculating cosine similarity
    var results =  Array[(Int, Double)]()

    for((docID, doc_vector) <- doc2Score) {
      var dotproduct: Double = 0
      for(idx <- doc_vector.indices)
        dotproduct += doc_vector(idx) * query_vector(idx)

      var length_q: Double = 0
      for(idx <- query_vector.indices)
        length_q += query_vector(idx) * query_vector(idx)
      length_q = Math.sqrt(length_q)

      var length_d: Double = 0
      length_d = norm_scores(docID)

      val length = length_q * length_d

      val cosineSimilarity = dotproduct/length
      results :+= (docID, cosineSimilarity)
    }
    results
  }
}

//3.3
object RankedSearch {

  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {

    val reuters = new QueryProcessor

    var index = ""

    //query with title mapping
    if (args.length == 2) {
      index = args(0)
      val titles = args(1)

      reuters.read(index)
      reuters.num_of_docs()
      reuters.calculate_doc_norms()

      //read in titles
      var titleMap = mutable.HashMap[Int, String]()
      val lines = Source.fromFile(titles).getLines()
      for (line <- lines) {
        val doc_id = line.split("\t")(0).toInt
        val title = line.split("\t")(1)
        titleMap += doc_id -> title
      }

      while (true) {
        val input = userinput()

        try {

          input.foreach(term => println(s"idf($term) = ${reuters.get_idf(term)}"))
          getTitles(titleMap, reuters.get_cos(input))
            .foreach(entry => println(entry._1 + " (" + entry._2 + "): " + entry._3))

        }
        //no results due to no entry found
        catch {case _: Throwable =>
          print("- No results -")
          sys.exit()
        }
      }
    }
    else help()
  }

  /**
    * Maps document identifiers to titles
    * @param titleMap map with doc id -> title stored
    * @param queryResults document identifiers
    * @return Array consisting of DocID, cosine similarity, title
    */
  def getTitles(titleMap: mutable.HashMap[Int, String],
                queryResults: Array[(Int, Double)]): Array[(Int, Double, String)] = {

    var titleMatches = Array[(Int, Double, String)]()

    //find matches
    queryResults.foreach(result => titleMatches :+= (result._1, result._2, titleMap(result._1)))
    titleMatches = titleMatches.sortWith(_._2 > _._2).take(5)
    titleMatches
  }


  /**
    * Help function for correct usage
    */
  def help() = {
    println("Usage: ./query-index arg1 arg2")
    println("\t\targ1: INPUT1 - inverted index file w/ frequencies")
    println("\t\targ2: INPUT2 - text file with doc id - title mapping")
    sys.exit()
  }

  def userinput() = {
    println()
    print("ranked-search: ")
    StdIn.readLine().split("\\s+").toList
  }
}