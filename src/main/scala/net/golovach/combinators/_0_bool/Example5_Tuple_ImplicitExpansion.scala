package net.golovach.combinators._0_bool

import net.golovach.combinators._0_bool.Bool._

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

object Example5_Tuple_ImplicitExpansion extends App {

  // ====================

  implicit def tupleBoolAlgebra[A: BooleanAlgebra, B: BooleanAlgebra] = new BooleanAlgebra[(A, B)] {
    override def &(p: (A, B), q: (A, B)): (A, B)  = (p, q) match {
      case ((p0, p1), (q0, q1)) => (p0 & q0, p1 & q1)
    }
    override def |(p: (A, B), q: (A, B)): (A, B)  = (p, q) match {
      case ((p0, p1), (q0, q1)) => (p0 | q0, p1 | q1)
    }
    override def ~(p: (A, B)): (A, B)  = p match {
      case (p0, p1) => (~p0, ~p1)
    }
  }

  val p: (Int => Bool, (String => String) => (Bool, (Int => Bool, Bool))) = ???
  val q: (Int => Bool, (String => String) => (Bool, (Int => Bool, Bool))) = ???

  val r = p & q

  // ∀A,∀B,∀C: (A, B, C) ~ (A, (B, C))
  // Address = String :: Int :: Int
  // User    = String :: Int :: Address ~ (String, (Int, (String, (Int, Int))))
}
