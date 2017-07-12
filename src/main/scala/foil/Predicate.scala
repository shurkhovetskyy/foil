package foil

import java.util

class Predicate(val arity: Int, val types: List[String], val order: List[String]) {
  
  def this(arity: Int = 1) {
    this(arity, Nil, Nil)
   
  }

 
}
