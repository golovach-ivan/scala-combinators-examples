package net.golovach.combinators._0_bool

import net.golovach.combinators._0_bool.Bool._

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

object Example4_Quantifiers extends App {

  // === Predicate List fold
  def ∀[A, B](xs: (A => Bool)*): A => Bool =
    (xs :\ ((_: A) => T: Bool)) (_ & _)
  def ∃[A](xs: (A => Bool)*): A => Bool =
    (xs :\ ((_: A) => F: Bool)) (_ | _)

  val allOf = ∀((_: Int) > 0, (_: Int) < 10, (_: Int) % 2 == 0)
  val anyOf = ∃((_: Int) > 0, (_: Int) < 10, (_: Int) % 2 == 0)
}
