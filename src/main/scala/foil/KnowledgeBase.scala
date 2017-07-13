package foil

object KnowledgeBase {
  
	private var baseHolder: Data = null
	private var posHolder: Data = null
	private var negHolder: Data = null
	
	def load {
		baseHolder = Data.apply("/foil/bg.data")
		posHolder = Data.apply("/foil/pos.data")
		negHolder = Data.apply("/foil/neg.data")
	}
	
	def print {
		println("Base Knowledge:")
		baseHolder.tupleMap.foreach(println(_))
		baseHolder.predicateMap.foreach(predicate => println(predicate._2.name + " " + predicate._2.arity))
	
		println("\nPositive:")
		posHolder.tupleMap.foreach(println(_))
		
		println("\nNegative:")
		negHolder.tupleMap.foreach(println(_))
	}
	
	def isTargetPredicate (predicate: String):Boolean = {
		posHolder.relations.contains(predicate)
	}
	
	def base = baseHolder
	def pos = posHolder
	def neg = negHolder
	
	
	
	

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