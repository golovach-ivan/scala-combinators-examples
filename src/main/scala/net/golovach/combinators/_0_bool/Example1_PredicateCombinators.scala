package net.golovach.combinators._0_bool

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import Bool._

/**
  * Произвольные унарный(ψ_) и бинарный(_⊙_) операторы для типа A можно для произвольного
  * типа B естественным образом распространить на функциональный тип B => A.
  * A ⊙ A → A
  * ∀B. (B => A) ⊙ (B => A) → (B => A)
  * (p: B => A) ⊙ (q: B => A) = b => p(b) ⊙ q(b)
  *
  * ψ(A)→ A
  * ∀B. ψ(B => A) → (B => A)
  * ψ(p: B => A) = b => ψ(p(b))
  *
  * Для произвольного типа B, тип B => Bool имеет естественную семантику
  * валидатора екземпляра типа B.
  * Таким образом мы получаем комбинаторы валидаторов.
  */
// ========== Predicate Algebra #1
object Example1_PredicateCombinators extends App {

  // define Predicate lib
  import Ordered._
  def gt[A: Ordering](min: A): A => Bool = _ > min
  def gte[A: Ordering](min: A): A => Bool = _ >= min
  def lt[A: Ordering](max: A): A => Bool = _ < max
  def lte[A: Ordering](max: A): A => Bool = _ <= max

  val in10to20or50to60 = (gte(10) & lte(20)) | (gte(50) & lte(60))
}
