package foil

import java.util.ArrayList
import scala.collection.mutable.Map

abstract class Predicate(val name: String) {

	var paramTypes: List[String] = null
	var arity = 0
	
	def setTuples (tuples: List[String]) {
		arity = if (tuples.isEmpty ) { 0 } else {tuples.size};
		paramTypes = tuples;
	}
}

object Predicate {
	def addToList(list: Map[String, Predicate],
			predicate : String,
			tuples: List[String]) {
		
			var predObject = getObject(predicate);
			predObject.setTuples(tuples);
			
			list += predicate -> predObject;
	}
	
	// Decide type of predicate and return its new instance.	
	private def getObject (predicate: String):Predicate = {
		if (KnowledgeBase.isTargetPredicate(predicate))
			new RuleBasedPredicate (predicate)
		else
			new BaseKnowledgePredicate(predicate)
	}
	
	def getRules () = {}
null
}

class BaseKnowledgePredicate(name: String) extends Predicate( name: String) {

}

class RuleBasedPredicate(name: String) extends Predicate(name: String) {

    /*
     * Rules describing new learned predicate.
     */
    var rules = new ArrayList[Rule]
    
    def getRules (): ArrayList[Predicate] = {
      
      var finalRules = new ArrayList[Predicate]
      
      // Iterate through all rules learned for the predicate (can be several).
      for (i<-0 until rules.size) {
        var rule = rules.get(i)
        // Predicates which constitute the rule.
        var rulePredicates = rule.getPredicates
        for (k<-0 until rulePredicates.size()) {
          // Extracting each predicate separately.
          var predicate = rulePredicates.get(k);
          // If predicate is rule-based, and not from knowledge base 
          // (i.e. target predicate) it has to be decomposed into
          // knowledge-based predicates.
          if (predicate.isInstanceOf [RuleBasedPredicate]) {
            // Get predicate that constitute the previous known rule for
            // rule-based predicate.
            var decomposedPredicates = rules.get(i-1).getPredicates
            var collectedRules = new ArrayList[Predicate]
            for (j<-0 until decomposedPredicates.size) {
              // Recursively collect the rules for each predicate.
              // Might require going several levels up.
              var decomposedPredicate = decomposedPredicates.get(j).asInstanceOf[RuleBasedPredicate]
              collectedRules.addAll(decomposedPredicate.getRules)
            }
            // Add rules collected instead of predicate which in this case is
            // probably target predicate and thus cannot be evaluated same way as
            // knowledge based predicates are.
            finalRules.addAll (collectedRules)
          }
          else // BaseKnowledge
            finalRules.add(predicate)
        }
      }
      finalRules
    }

	/*def resolve(terms: List[(Member, Var)]) {
    from trimlogic.algorithm import unify
    println(this + "._resolve( " + str(terms) + " ) ")
    for rule in self.rules:
      rule = rule.instantiate()
      logging.debug("considering rule: " + str(rule))
      Head, Body, variables = rule.terms, rule.body, rule.variables
      mgu = unify(terms, Head, {})
      if mgu != None: 
        logging.debug("substitutions: " + str(mgu))
        yield (mgu, list(Body), variables)
  }
  
  def add_rule(self, Head=None, Body=None) {
    if self.arity == None: self.arity = len(Head)
    if Body == None: self.rules.append(Fact(self, Head))
    else: self.rules.append(Rule(self, Head, Body))
  }
  
  override def toString() : String = {
    if (this.name.isEmpty) {
      
    }
  }
  
  def __str__(self) {
    if self.name != None: return self.name
    else: return object.__str__(self)
  }*/
}