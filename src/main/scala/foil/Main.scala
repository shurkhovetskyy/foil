package foil

import scala.io.Source
import java.util.ArrayList


object Main extends App {

  var DEBUG = true
	KnowledgeBase.load
	
	if (DEBUG) {
	  KnowledgeBase.print
	}
  
	var targetPredicates = KnowledgeBase.generateTargetVariables
	var candidates = KnowledgeBase.generateCandidates
	
	targetPredicates.foreach(target => {

    var targetVars = target._2 // get variables list for the target predicate
			  
	  candidates.foreach(candidate => {
	    val predicate = candidate._1 // obtain right-side predicate name
	    val varsCombinations = candidate._2 // and all its possible variables combinations
	    val iterator = varsCombinations.iterator() 
	    
      debug("\nTarget variables position: ")
	    while (iterator.hasNext())  { // move all over variables combinations of the right-side predicate
	      val rightSideVars = iterator.next()
	      debug("\n" + targetVars + " -> " + rightSideVars)
	     
	      val positionList = new ArrayList[(Int, Int)]
	      for (targetVarPosition <- 0 until targetVars.size()) {
	        val targetVar = targetVars.get(targetVarPosition)
	        val varPosition = findTargetVariblePosition(targetVar, rightSideVars) 
	        
	        if (varPosition > -1) { // target variable exists on the right side 
	          // they both must match in base knowledge and target predicate tuples
	          // TODO: add variable name to position list or nor?
	          positionList.add((targetVarPosition, varPosition))
	        }
	      }
	      debug("Positions list:\n" + positionList)
	    }
	    
	  })
	})
	
	/*
	 * Finds target variable position in the right-side predicate variables list
	 */
	def findTargetVariblePosition(targetVariable: Term, rightSideRuleVariables: ArrayList[Term]) : Int = { 
    // find target variable position in the variables list of right-side predicate
    val position = rightSideRuleVariables.indexOf(targetVariable) 
    //debug(targetVariable + " " + position)
    position
	}
	
	//foil(bKnowledge)
//	println(getTargetPredicate(epos))
	
	// Predicates learned so far - none.
	var learnedPredicates = Set.empty[String];

	def foil(B_knowledge: Data, target: Predicate) = {
		
		var remPos = KnowledgeBase.pos;	// Remaining positive examples.
		// Pool of predicates from which candidates can be generated.
		// Includes predicates from base knowledge plus learned
		// predicates if any.
		var predPool = KnowledgeBase.base.relations.union(learnedPredicates);
		
		while	(remPos != Set.empty) {
			var newRule = Set.empty[String];
			var newRuleNeg = KnowledgeBase.neg;
			while (newRuleNeg != Set.empty) {
			  var target = KnowledgeBase.generateTargetVariables
			  var candidates = KnowledgeBase.generateCandidates
			  
			  candidates.foreach(candidate => {
			    val predicate = candidate._1 // obtain predicate name
			    val varsCombinations = candidate._2 // and all possible variables combinations
			    val iterator = varsCombinations.iterator()
			    
			    while (iterator.hasNext()) {
			      val variables = iterator.next()
			      println(variables)
			    }
			    
			    
			    
			    
			  })
			  
				// 1. candidate_literals = generate candidates()
				// 2. best_literal = foil_gain (L, newrule)
				// 3. add best_literal to new_rule preconditions
				// 4. newruleneg = subset of newrulenew that satisfies newrule preconditions.
	
			}
	
			// Add learned rule-based predicate to the pool of
			// learned predicates.
			learnedPredicates = learnedPredicates.union(newRule);
			// epos = epos - (members of pos covered by newrule)
	
			// return new rule
		}
	}
	
	

//	def getTargetPredicate(epos: Data): String = {
//		var targetPredicate = epos.relations(0) + "("
//		for (i <- 1 to epos.tupplesSize) {
//			targetPredicate += "X" + i + (if (i < epos.tupplesSize) { ", " } else { "" })
//		}
//		targetPredicate += ")"
//		targetPredicate
//	}

	def findMaximalGeneralHornClause(B_knowledge: Data, epos: Data, eneg: Data) = {

	}
	
	def debug(out: String) {
	   if (DEBUG)
	      println(out)
	}
}




















