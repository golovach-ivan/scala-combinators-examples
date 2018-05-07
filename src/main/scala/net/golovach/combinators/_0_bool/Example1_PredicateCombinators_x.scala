package net.golovach.combinators._0_bool

import net.golovach.combinators._0_bool.Bool._

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

object Example1_PredicateCombinators_x extends App {

  // define Predicate lib
  import Ordered._
  def gte[A: Ordering](min: A): A => Bool = _ >= min
  def lte[A: Ordering](max: A): A => Bool = _ <= max

  implicit class X[A: Ordering](point: A) {
    def |->(): A => Bool = _ >= point
  }
  def <-|[A: Ordering](point: A): A => Bool = _ <= point

  val in10to20or50to60a = (gte(10) & lte(20)) | (gte(50) & lte(60))
  val in10to20or50to60b = (10.|-> & <-|(20)) | (50.|-> & <-|(60))
}
