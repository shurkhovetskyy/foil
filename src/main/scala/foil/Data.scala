package foil

import scala.io.Source
import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable.Map

/**
 * Class for getting a list of prolog clauses into a "Data" format, for use by our learner

 * Created by m4b on 4/29/14.
 */
class Data (val clauses: java.util.List[Relation]){

  val tupples = getConstants.toArray
  val relations = getRelations
  val predicateMap = getPredicateMap
  val tupleMap = getTupleMap
  val tupplesSize = getTuplesSize // p/n
  val numTuples = tupples.size // all tuples of the predicate
  val numRelations = relations.size

  private def getPredicateMap = {
    val predicateMap = Map.empty[String, Predicate]
    for (clause <- this.clauses){
      if (!predicateMap.contains(clause.predicate)) {
      	Predicate.addToList(predicateMap, clause.predicate, clause.tupples)
      //	predicateMap += clause.predicate -> new Predicate(clause.predicate, clause.tupples)
      }
    }
    predicateMap
  }
  
  private def getTupleMap = {
    // TODO: make string and List[List[String]] separate types, Relation and  RelationArguments/Tuples/RelationLiterals, respectively
    val tupleMap = Map.empty[String, List[List[String]]]
    for (clause <- this.clauses){
      if (tupleMap.contains(clause.predicate)){
        tupleMap += clause.predicate -> (clause.tupples :: tupleMap(clause.predicate))
      }else{
        tupleMap += clause.predicate -> List(clause.tupples)
      }
    }
    tupleMap
  }

  /**
   * yea, we loop over this twice (thrice)...
   * @return
   */
  private def getRelations = {
    var set = Set.empty[String]
    for (clause <- this.clauses){
      set += clause.predicate
    }
    set
  }

  private def getTuplesSize = {
    this.clauses.head.tupples.length
  }
  
  private def getConstants = {
    var set = Set.empty[String]
    for (clause <- this.clauses if !clause.tupples.isEmpty){
      clause.tupples.foreach(s => set += s)
    }
    set
  }

}

object Data {

  def test = apply("/kinship.data")

  def apply(path: String): Data = {
    val l = new util.ArrayList[Relation]()
    val res = Source.fromURL(getClass.getResource(path))
    for (line <- res.getLines() if !line.isEmpty) l add Relation.parse(line)
    new Data(l)
  }

  /**
   * this is just for unit testing
   * @param unit the unit test
   * @return the data based on the unit test
   */
  def apply(unit: List[String]): Data = {
    val l = new util.ArrayList[Relation]()
    for (line <- unit)
      if (!line.isEmpty)
        l add Relation.parse(line)
    new Data(l)
  }

}