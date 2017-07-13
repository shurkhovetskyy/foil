package foil

class Term(var name:String) {
   override def toString() : String = {
    name
  }
  
  def canEqual(that: Any) = that.isInstanceOf[Term]
  def apply_bindings(bindings: Map[String, List[List[String]]]) : Term = {
    this
  }
}

class Atom(name:String) extends Term(name:String)  {
    
  override def hashCode = {
    val hash =  if (name.isEmpty()) 0 else name.hashCode
    super.hashCode + hash
  }
  override def equals(other: Any): Boolean = other match { 
    case that: Atom => that.canEqual(this) && this.name == that.name
    case _ => false 
  }
  
  // TODO: check
  override def apply_bindings(bindings: Map[String, List[List[String]]]) : Term = {
  //TODO: check
    //if bindings .has_key(self): return bindings[self]
    //else: return self
    this
  }
}

class Var(name:String) extends Term(name:String)  {

  var scope: List[String] = null
  
  def this(name: String, scope: List[String]) {
    this(name)
    this.scope = scope
  }
  
  override def equals(other: Any): Boolean = other match { 
   // case that: Term => that.canEqual(this) and this.name == other.name
    case _ => false 
  }
  
  // TODO: check
  override def apply_bindings(bindings: Map[String, List[List[String]]]) : Term = {
    /*if bindings.has_key(self): return bindings[self]
    else: return self*/
    this
  }
}
  
object Var {
  var unique_count: Int = 0

  def get_unique(variable: Var) : Var = {
    Var.unique_count += 1
    new Var("@_" + Var.unique_count + "_" + variable.name)
  }
}

class VariableFactory {  
  def getVariable(name: String) : Var = {
    new Var(name)
  }
}
  
  
class AtomFactory {
  def getAtom(name: String) : Atom = {
    new Atom(name)
  }
}

class Pred (name:String) extends Term(name:String)  {
  
  var predicate : Predicate = null
  
  def this(name: String, predicate : Predicate, terms: (Member, Var)*) {
    this(name)
    this.predicate = predicate
    /*this.terms = terms
    this.variables = None*/
  }
  
  /*def __repr__(self):
    return str(self)
  
  def __str__(self):
    if len(self.terms) > 0: return str(self.predicate) + "(" + str(reduce(lambda x,y: str(x) + ", " + str(y), self.terms)) + ")"
    else: return str(self.predicate)
    
  def __hash__(self):
    return hash(self.predicate.name) + len(self.terms)
  
  def apply_bindings(self, bindings):
    new_terms = []
    for term in self.terms:
      if isinstance(term, Term): new_terms.append(term.apply_bindings(bindings))
      else: new_terms.append(term)
    return self.predicate(*new_terms)*/
}