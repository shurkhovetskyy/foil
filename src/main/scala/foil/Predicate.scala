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
null
}

class BaseKnowledgePredicate(name: String) extends Predicate( name: String) {

}

class RuleBasedPredicate(name: String) extends Predicate(name: String) {


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