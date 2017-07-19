package foil

import scala.io.Source
import java.util.ArrayList
import scala.collection.mutable.Map

object Main extends App {

  var DEBUG = true
	KnowledgeBase.load
	KnowledgeBase.print

  
	var targetPredicates = KnowledgeBase.generateTargetVariables
	var candidates = KnowledgeBase.generateCandidates
	var bodyPredicates = Map.empty[ArrayList[Term], String] // predicates to be added to the body
	        
	targetPredicates.foreach(target => {

	  candidates.foreach(candidate => {
	    
	    val predicateName = candidate._1 // obtain right-side predicate name
	    val varsCombinations = candidate._2 // and all its possible variables combinations
	    val iterator = varsCombinations.iterator() 
	    
	    var wig = 0d
      while (iterator.hasNext())  { // move all over variables combinations of the right-side predicate
	      val rightSideVars = iterator.next()
	      val predicates = updateRuleBody(bodyPredicates, predicateName, rightSideVars)

	      debug("\nBody: " + predicates)
	      
	      val gain = KnowledgeBase.calculateGain(target, predicates)
	      if (gain > wig) {
	        wig = gain
	        debug("Gain: " + gain)
	        bodyPredicates(rightSideVars) = predicateName
	      }
	    }
	    
	    
	  })
	})
	
	println(bodyPredicates)

  def updateRuleBody(bodyPredicates: Map[ArrayList[Term], String], predicateName: String, predicateVars: ArrayList[Term]) = {
    // clone map with body predicates
    val updatedBody = Map[ArrayList[Term], String]() ++= bodyPredicates
    updatedBody(predicateVars) = predicateName
    updatedBody
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




















