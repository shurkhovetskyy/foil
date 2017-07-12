package foil

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Represents an K-ary predicate + set of tupples
 *
 * Created by m4b on 4/24/14.
 */
class Relation (val predicate: String, val tupples: List[String]) {
  require (!tupples.isEmpty)

  def apply(i: Int) = if (i <= 0) predicate else tupples(i-1)

  def len = tupples.length

  override def toString = predicate + "(" + tupples.mkString(",") + ")"

}

object Relation extends JavaTokenParsers {

  def apply(s: String): Relation = parse(s)

  private def literal: Parser[Relation] = predicate~tupples ^^ (x => new Relation(x._1, x._2))

  private def predicate: Parser[String] = ident

  private def tupples: Parser[List[String]] = "("~>repsep(ident, ",")<~")"

  def parse(s: String): Relation = {
    parseAll(literal, s).get
  }

}
