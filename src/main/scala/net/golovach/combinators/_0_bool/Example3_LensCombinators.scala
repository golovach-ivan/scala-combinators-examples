package net.golovach.combinators._0_bool

import net.golovach.combinators._0_bool.Bool._

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

// AlgDT Validator Combinators
object Example3_LensCombinators extends App {

  // define Predicate lib
  import Ordered._
  def gt[A: Ordering](min: A): A => Bool = _ > min
  def gte[A: Ordering](min: A): A => Bool = _ >= min
  def lt[A: Ordering](max: A): A => Bool = _ < max
  def lte[A: Ordering](max: A): A => Bool = _ <= max

  // === Business Model - Algebraic data Types
  case class Address(street: String, house: Int, flat: Int)
  case class User(name: String, age: Int, address: Address)

  val checkName: String => Bool = _.length > 3
  val checkStreet: String => Bool = _.length > 3
  val positive: Int => Bool = gte(0)

  class ω[A] {
    def apply[B](f: A => B) = new {
      def *>[C](p: B => C): (A => C) = a => p(f(a))
    }
  }
  object ω {
    def apply[A]: ω[A] = new ω[A]
  }

  val checkAddress: Address => Bool =
    (ω[Address](_ street) *> checkStreet) &
      (ω[Address](_ house) *> positive) &
      (ω[Address](_ flat) *> positive)

  val checkUser: User => Bool =
    (ω[User](_ name) *> checkName) &
      (ω[User](_ age) *> positive) &
      (ω[User](_ address) *> checkAddress)

  val checkUserX: User => Bool =
    (ω[User](_ name) *> checkName) &
      (ω[User](_ age) *> positive) &
      (ω[User](_.address.street) *> checkStreet) &
      (ω[User](_.address.house) *> positive) &
      (ω[User](_.address.flat) *> positive)
}
