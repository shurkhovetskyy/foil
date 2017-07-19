package foil

import java.util.ArrayList

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
	def find(targetName: String, predicateName: String, positionList: ArrayList[(Int, (Int, Term))]) = {
	  val positive = posHolder.tupleMap(targetName)
	  val negative = negHolder.tupleMap(targetName)
	  val rightSide = baseHolder.tupleMap(predicateName)
	  
	  Main.debug("\nFind variable occurences:")
	  val posEntropy = findVariable(positive, rightSide, positionList)
	  val nedEntropy =findVariable(negative, rightSide, positionList)
	}
	
	/* term object can be variable or atom
	 * in case of Var object we have to check that all Var objects for both target and right-side predicate must match  in find() method
	 * and we do not care about Atom objects
	 */
	def findVariable(targetTuples: List[List[String]], rightSideTuples: List[List[String]], positionList: ArrayList[(Int, (Int, Term))]) = {
	  Main.debug(targetTuples.toString())
	  Main.debug(rightSideTuples.toString() + "\n")
	  //Main.debug(positionList.toString())
	  
    var N = 0 // n++ TODO: check it !!!
    var n = 0 // n+
    targetTuples.foreach(targetTuple => {
      var N_added = false
      
	    rightSideTuples.foreach(rightSideTupple => {
	      var n_added = true
	      for (index <- 0 until positionList.size()) {
	        
	        
	        val current = positionList.get(index)
	        val term = current._2._2 // obtain Var or Atom object

	        n_added = n_added && (targetTuple(current._1) == rightSideTupple(current._2._1)) && term.isInstanceOf[Var]
	        if (n_added)
	          Main.debug(current + " " + targetTuple + " " + rightSideTupple)
	      }
	      
	      if (n_added) {
	        n += 1
          if (!N_added) {
            N += 1
            N_added = true
          }
	      }
	    })
	  })
	  Main.debug("n++ = " + N + "; n+ = " + n)
	  (N, n)
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
		    list.add(new Var("X1"))
		    list.add(new Var("X2"))
		    candidates.add(list)
		    
		    list = new ArrayList[Term]
		    list.add(new Var("X2"))
		    list.add(new Var("X1"))
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