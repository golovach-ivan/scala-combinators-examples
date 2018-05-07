package net.golovach.combinators._0_bool

import scala.language.implicitConversions

object Bool extends Enumeration {
  type Bool = Value
  val T, F, ? = Value

  implicit def boolean2bool(b: Boolean): Bool = if (b) T else F

  trait BooleanAlgebra[A] {
    def &(p: A, q: A): A
    def |(p: A, q: A): A
    def ~(p: A): A
  }

  implicit class BoolAlgebraOps[A](p: A)(implicit alg: BooleanAlgebra[A]) {
    def &(q: A): A = alg.&(p, q)
    def |(q: A): A = alg.|(p, q)
    def unary_~(): A = alg.~(p)
  }

  implicit val boolBooleanAlgebra = new BooleanAlgebra[Bool] {
    override def &(p: Bool, q: Bool): Bool = (p, q) match {
      case (T, T) => T
      case (F, _) => F
      case (_, F) => F
      case (_, _) => ?
    }

    override def |(p: Bool, q: Bool): Bool = (p, q) match {
      case (F, F) => F
      case (T, _) => T
      case (_, T) => T
      case (_, _) => ?
    }

    override def ~(p: Bool): Bool = p match {
      case F => T
      case T => F
      case _ => ?
    }
  }

  // define bool algebra for functional type ∀A,B:BoolAlgebra: A => B
  implicit def funcBooleanAlgebra[A, B: BooleanAlgebra] = new BooleanAlgebra[A => B] {
    override def &(p: A => B, q: A => B): A => B = a => p(a) & q(a)
    override def |(p: A => B, q: A => B): A => B = a => p(a) | q(a)
    override def ~(p: A => B): A => B = a => ~p(a)
  }
}
