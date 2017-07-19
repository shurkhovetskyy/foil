package foil

import java.util.ArrayList
import scala.collection.mutable.Map

object KnowledgeBase {
  
	private var baseHolder: Data = null
	private var posHolder: Data = null
	private var negHolder: Data = null
	
	def declarePositiveExamples() {
	  
	}
	
	def load {
	  posHolder = Data.apply("/foil/pos.data")
		negHolder = Data.apply("/foil/neg.data")
		baseHolder = Data.apply("/foil/bg.data")
	}
	
	def print {
	  if (!Main.DEBUG)
	    return
		println("Base Knowledge:")
		baseHolder.tupleMap.foreach(println(_))
		baseHolder.predicateMap.foreach(predicate => println(predicate._2.name + " " + predicate._2.arity))
	
		println("\nPositive:")
		posHolder.tupleMap.foreach(println(_))
		
		println("\nNegative:")
		negHolder.tupleMap.foreach(println(_))

	  println("\nTarget variables:")
	  println(generateTargetVariables)
	  
	  println("\nCandidates generation:")
	  println(generateCandidates)
	}
	
	def isTargetPredicate (predicate: String):Boolean = {
	  if (posHolder != null) {
		  posHolder.relations.contains(predicate)
	  } else {
	    true
	  }
	}
	
	def base = baseHolder
	def pos = posHolder
	def neg = negHolder
	
	/*
	 * Finds variables matching using position in predicate variables list
	 */
	def calculateGain(target: (String, ArrayList[Term]), bodyPredicates: Map[ArrayList[Term], String]) = {
	  
	  Main.debug("\nFind variable occurences:")
	  
	  // TODO: calculate correctly - update bknowledge
	  // obtain n+, n- from previous T+, T-
	  
	  // calculate current n+, n-
	  matchTuples(target, bodyPredicates)
	}
	
	/*
	 * number of bits to signal that tuple is positive/negative
	 */
	def entropy(n_pos: Double, n_neg: Double) = {
	  - math.log(n_pos / (n_pos + n_neg)) / math.log(2)
	}
	
	/*
	 * weighted inomation gain
	 * ic_i, ic_(i+1), n_i_++
	 */
	def wig(ic_prev: Double, ic_next: Double, n_i: Double) = {
	  n_i * (ic_prev - ic_next)
	}
	
	
	def calc(targetTuples: List[List[String]], target: (String, ArrayList[Term]), bodyPredicates: Map[ArrayList[Term], String]) = {
	  var N = 0 // n++_(i) TODO: check it !!!
    var n = 0 // n+_(i+1)

    bodyPredicates.foreach(predicate => {	   
      
      val predicateTuples = baseHolder.tupleMap(predicate._2)
      val positionList = Term.positionList(target, predicate._1)
      
      Main.debug(predicate.toString())
      Main.debug(positionList + " " + targetTuples)
      Main.debug(predicateTuples.toString())
      
      targetTuples.foreach(targetTuple => {
        
        var N_added = false
  	    predicateTuples.foreach(tuple => {
  	      var n_added = true
  	      for (index <- 0 until positionList.size()) {
  	        val current = positionList.get(index)
  	        val term = current._2._2 // obtain Var or Atom object
  
  	        n_added = n_added && (targetTuple(current._1) == tuple(current._2._1)) && term.isInstanceOf[Var]
  	        
  	        //Main.debug(targetTuple(current._1) + " " + tuple(current._2._1) + " " + n_added)
  	      }

  	      if (n_added) {
  	        Main.debug(positionList + " " + targetTuple + " " + tuple)
  	        n += 1
            if (!N_added) {
              N += 1
              N_added = true
            }
  	        //Main.debug("n++ = " + N + "; n+ = " + n)
  	      }
  	    })
  	    
  	    
  	  })
	  })
	  Main.debug("n++ = " + N + "; n+ = " + n)
	  (N, n)
	}
	
	
	/* 
	 * Term object can be variable or atom
	 * in case of Var object we have to check that all Var objects for both target and right-side predicate must match
	 */
	def matchTuples(target: (String, ArrayList[Term]), bodyPredicates: Map[ArrayList[Term], String]) = {
	  
	  val targetName = target._1
	  val positive = posHolder.tupleMap(targetName)
	  val negative = negHolder.tupleMap(targetName)

	  val n_pos_i = positive.size
	  val n_neg_i = negative.size
	  val ic_prev = entropy(n_pos_i, n_neg_i)
	  
	  val pos = calc(positive, target, bodyPredicates)
    val neg = calc(negative, target, bodyPredicates)
	  
	  
    var gain = 0d
    if (pos._2 != 0 &&  neg._2 != 0) {
	    val ic_next = entropy(pos._2, neg._2)
	    gain = wig(ic_prev, ic_next, pos._1) 
	    Main.debug(n_pos_i + " " + n_neg_i + " " + ic_prev + " " + ic_next + " " + gain)
	  }
	  gain    
	}
	
	/*
	 * Variable generation for the left part of the rule
	 * */
	def generateTargetVariables = {
	  var target = scala.collection.mutable.Map.empty[String, ArrayList[Term]]
	  posHolder.predicateMap.foreach(predicate => {
	    val name  = predicate._2.name
	    val arity = predicate._2.arity
		  var list = new ArrayList[Term]
		  for( v <- 1 to arity)
         list.add(new Var("X" + v))
      target(name) = list
	  })
	  target
	}
	
	/*
	 * All possible variable combinations of base knowledge predicates
	 * TODO: update it with target predicate when new rule was generated
	 * */
	def generateCandidates = {
	  var result = scala.collection.mutable.Map.empty[String, ArrayList[ArrayList[Term]]]
	  baseHolder.predicateMap.foreach(predicate => {
	    var candidates = new ArrayList[ArrayList[Term]]
	    val name  = predicate._2.name
	    val arity = predicate._2.arity
		  // TODO: generate all candidates correctly
		  if (arity == 2) {
		    var list = new ArrayList[Term]
		    list.add(new Var("X2"))
		    list.add(new Var("X1"))
		    candidates.add(list)
		    
		    list = new ArrayList[Term]
		    list.add(new Var("X1"))
		    list.add(new Var("X2"))
		    candidates.add(list)
		    
		    list = new ArrayList[Term]
		    list.add(new Var("X1"))
		    list.add(new Atom("Y1")) // we don't care about this variable in the target predicate
		    candidates.add(list)
		    
		    list = new ArrayList[Term]
		    list.add(new Var("X2"))
		    list.add(new Atom("Y1")) // we don't care about this variable in the target predicate
		    candidates.add(list)
		  } 
		  else if (arity == 1) {
		    var list = new ArrayList[Term]
		    list.add(new Var("X1"))
		    candidates.add(list)
		    
		    list = new ArrayList[Term]
		    list.add(new Var("X2"))
		    candidates.add(list)
		  }
	  	    
	    result(name) = candidates
	  })
		result
	}
	

//	def getItem(key: String): Predicate = {
//		//if this._map.has_key(key):
//		// raise "Predicate with name '" + predicate.name + "' already exists." Exception
//		this.base.getOrElse(key, null)
//	}
//
//	def remove(predicate: Predicate) {
//		this.base -= predicate.name
//	}
//
//	def add(predicate: Predicate) {
//		this.base(predicate.name) = predicate
//	}
//
//	def addAll(list: List[Predicate]) {
//		list.foreach(predicate => this.add(predicate))
//	}
}