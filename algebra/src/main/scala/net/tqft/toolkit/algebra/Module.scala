package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra._

trait ModuleOverRig[A, B] extends AdditiveMonoid[B] {
  def scalarMultiply(a: A, b: B): B
}

trait Module[A, B] extends ModuleOverRig[A, B] with AdditiveGroup[B]

