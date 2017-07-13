package foil

import scala.io.Source
import java.util.ArrayList

object Main extends App {

	val bKnowledge = Data.apply("/foil/bg.data")
	val epos = Data.apply("/foil/pos.data")
	val eneg = Data.apply("/foil/neg.data")
	
	println("Base Knowledge:")
	bKnowledge.tupleMap.foreach(println(_))
	bKnowledge.predicateMap.foreach(predicate => println(predicate._2.name + " " + predicate._2.arity))

	println("\nPositive:")
	epos.tupleMap.foreach(println(_))
	
	println("\nNegative:")
	eneg.tupleMap.foreach(println(_))
	
	//foil(bKnowledge)
//	println(getTargetPredicate(epos))
	
	// Predicates learned so far - none.
	var learnedPredicates = Set.empty[String];

	def foil(B_knowledge: Data, target: Predicate) = {
		
		var remPos = epos;	// Remaining positive examples.
		// Pool of predicates from which candidates can be generated.
		// Includes predicates from base knowledge plus learned
		// predicates if any.
		var predPool = bKnowledge.relations.union(learnedPredicates);
		
		while	(remPos != Set.empty) {
			var newRule = Set.empty[String];
			var newRuleNeg = eneg
			while (newRuleNeg != Set.empty) {
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
	
	def generateCandidates (predPool: Set[String]) {
		bKnowledge.predicateMap.foreach(predicate => {
		  val arity = predicate._2.arity
		  
		})
		
		
	}
}




















