package foil

import scala.io.Source
import java.util.ArrayList

object Main extends App {

	val bKnowledge = Data.apply("/foil/bg.data")
	val epos = Data.apply("/foil/pos.data")
	val eneg = Data.apply("/foil/neg.data")
	println(epos.tupplesSize)
	bKnowledge.tupleMap.foreach(println(_))

	//foil(bKnowledge)
	println(getTargetPredicate(epos))

	def foil(B_knowledge: Data) = {

		var newRule = {}
		var newRuleNeg = eneg
		while (newRuleNeg != Set.empty) {
			// 1. candidate_literals = generate candidates()
			// 2. best_literal = foil_gain (L, newrule)
			// 3. add best_literal to new_rule preconditions
			// 4. newruleneg = subset of newrulenew that satisfies newrule preconditions

		}

		// learned_rules = learned_rules + newrule
		// epos = epos - (members of pos covered by newrule)

		// return new rule

	}

	def getTargetPredicate(epos: Data): String = {
		var targetPredicate = epos.relations(0) + "("
		for (i <- 1 to epos.tupplesSize) {
			targetPredicate += "X" + i + (if (i < epos.tupplesSize) { ", " } else { "" })
		}
		targetPredicate += ")"
		targetPredicate
	}

	def findMaximalGeneralHornClause(B_knowledge: Data, epos: Data, eneg: Data) = {

	}
}




















