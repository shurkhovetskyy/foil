package foil

import java.util.ArrayList
import scala.collection.mutable.Map

abstract class Predicate(var arity: Int, var types: List[String], var order: List[String]) {
  
  var name: String = ""
  var rules: List[String] = null
  var param_types: List[String] = null
  def this(arity: Int = 1) {
    this(arity, null, null)
   
  }
}

class KnowledgeBase {
  
  val base : Map[String, Predicate] = null
  
  def getItem(key: String) : Predicate = {
    //if this._map.has_key(key):
     // raise "Predicate with name '" + predicate.name + "' already exists." Exception
    this.base.getOrElse(key, null)
  }
  
  def remove(predicate: Predicate) {
     this.base -= predicate.name
  }
  
  def add(predicate: Predicate) {
    this.base(predicate.name) = predicate
  }

  def addAll(list: List[Predicate]) {
     list.foreach(predicate => this.add(predicate))
  }
  
}


class RuleBasedPredicate extends Predicate {
  
  
  def this(name: String = "", types: List[String] = null) {
    this()
    this.name = name    
    if (types == null || types.isEmpty) {
      this.arity = 0
    } else {
      this.param_types = types
      this.arity = types.size
    }
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