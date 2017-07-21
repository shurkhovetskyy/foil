package foil

import scala.io.Source
import java.util.ArrayList
import scala.collection.mutable.Map

object Main extends App {

  var DEBUG = true
	KnowledgeBase.load
	KnowledgeBase.print
	KnowledgeBase.foil
	
	def debug(out: String) {
	   if (DEBUG)
	      println(out)
	}
}




















