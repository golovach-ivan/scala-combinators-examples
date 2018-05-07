package net.golovach.combinators._0_bool

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import Bool._

/**
  * Generators:{`|-|`, `<-|`, `|->`, `<->`}
  * Combinators: {`∩`, `∪`, `\`}
  */
object Example2_LineSubsetCombinators extends App {

  // define Predicate lib
  import Ordered._
  def gt[A: Ordering](min: A): A => Bool = _ > min
  def gte[A: Ordering](min: A): A => Bool = _ >= min
  def lt[A: Ordering](max: A): A => Bool = _ < max
  def lte[A: Ordering](max: A): A => Bool = _ <= max

  // redefine Predicate Algebra Ops: {'|' => '∪', '&' => '∩'}, add operator '\'.
  implicit class LineSubsetOps[A: BooleanAlgebra](p: A) {
    def ∩(q: A): A = implicitly[BooleanAlgebra[A]].&(p, q)
    def ∪(q: A): A = ~(~p ∩ ~q)
    def \(q: A): A = p ∩ ~q
  }
  implicit class IntervalOps[A: Ordering](min: A) {
    /** Interval: (min ... max) */
    def <->	(max: A) = gt(min) & lt(max)
    /** Interval: [min ... max) */
    def |->	(max: A) = gte(min) & lt(max)
    /** Interval: (min ... max] */
    def <-|	(max: A) = gt(min) & lte(max)
    /** Interval: [min ... max] */
    def |-|	(max: A) = gte(min) & lte(max)
  }

  def ∘(point: Int) = point |-| point

  val in10to20or50to6a = (10 |-| 20) ∪ (50 |-| 60) \ ∘(55)
  val in10to20or50to6b = (10 |-| 60) \ (20 <-> 50)
}
