package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.permutations.Permutations.Permutation

object GroupActions {
  def trivialAction[A, B]: GroupAction[A, B] = new GroupAction[A, B] {
    def act(a: A, b: B) = b
  }
  def permutationAction[C]: GroupAction[Permutation, Seq[C]] = new GroupAction[Permutation, Seq[C]] {
    import net.tqft.toolkit.permutations.Permutations._

    def act(a: Permutation, b: Seq[C]) = a permute b
  }
  def conjugationAction[A](group: Group[A]) = new GroupAction[A, A] {
    def act(a: A, b: A) = group.multiply(group.inverse(a), b, a)
  }
}


