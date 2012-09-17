package net.tqft.toolkit.algebra.categories

import net.tqft.toolkit.algebra._

trait NLinearCategory[O, M] extends Category[O, M] { nlc =>
  def zeroMorphism(o1: O, o2: O): M
  def add(x: M, y: M): M

  protected class EndomorphismRig(o: O) extends Rig[M] {
    override def zero = nlc.zeroMorphism(o, o)
    override def one = nlc.identityMorphism(o)
    override def multiply(x: M, y: M) = nlc.compose(x, y)
    override def add(x: M, y: M) = nlc.add(x, y)
  }

}

