package net.tqft.toolkit.algebra.khovanov

import scala.language.implicitConversions

case class Crossing(sign: Int, strands: (Int, Int, Int, Int))
case class Arc(labels: Int*)
case class Tangle(crossings: Seq[Crossing], arcs: Seq[Arc])
case class Cobordism(source: Tangle, target: Tangle, frames: Seq[ElementaryCobordism])

sealed trait ElementaryCobordism {
  def source: Tangle = ??? // TODO remove the implementation here
  def target: Tangle = ??? // TODO remove the implementation here
}
case class CreateCircle(arc: Int) extends ElementaryCobordism
case class DestroyCircle(arc: Int) extends ElementaryCobordism
case class Saddle(se: Int, ne: Int, nw: Int, sw: Int) extends ElementaryCobordism
case class Twist(incoming: Int, loop: Int, outgoing: Int) extends ElementaryCobordism
case class Untwist(incoming: Int, loop: Int, outgoing: Int) extends ElementaryCobordism
case class R2() extends ElementaryCobordism
case class R3() extends ElementaryCobordism

object MovieMoves {
  def all = MM1 #:: MM2 #:: MM3 #:: MM4 #:: MM5 #:: MM6 #:: MM7 #:: MM8 #:: MM9 #:: MM10 #:: MM11 #:: MM12 #:: MM13 #:: MM14 #:: MM15 #:: Stream.empty[(Cobordism, Cobordism)]
  // for now, let's just pick a representative of each one, and not worry about all the variants
  lazy val MM1 = {
    val source = Tangle(Seq.empty, Seq(Arc(1, 2)))
    val target = source
    val left = Cobordism(source, target, Seq(Twist(1, 3, 2), Untwist(1, 3, 2)))
    val right = Cobordism(source, target, Seq.empty)
    (left, right)
  }
  lazy val MM2 = (???, ???)
  lazy val MM3 = (???, ???)
  lazy val MM4 = (???, ???)
  lazy val MM5 = (???, ???)
  lazy val MM6 = (???, ???)
  lazy val MM7 = (???, ???)
  lazy val MM8 = (???, ???)
  lazy val MM9 = (???, ???)
  lazy val MM10 = (???, ???)
  lazy val MM11 = (???, ???)
  lazy val MM12 = (???, ???)
  lazy val MM13 = (???, ???)
  lazy val MM14 = (???, ???)
  lazy val MM15 = (???, ???)
}

object Cobordism {
  implicit def apply(e: ElementaryCobordism): Cobordism = Cobordism(e.source, e.target, Seq(e))

  implicit object Cobordisms extends TensorCategory[Tangle, Cobordism] {
    def compose(x: Cobordism, y: Cobordism): Cobordism = {
      require(x.target == y.source) // perhaps we should make this more general, like we do for cans?
      Cobordism(x.source, y.target, x.frames ++ y.frames)
    }
    def identity(o: Tangle): Cobordism = Cobordism(o, o, Nil)
    def source(x: Cobordism): Tangle = x.source
    def target(x: Cobordism): Tangle = x.target

    def tensorMorphisms(x: Cobordism, y: Cobordism): Cobordism = ???
    def tensorObjects(x: Tangle, y: Tangle): Tangle = ???
  }
}

case class AnnularTangle(cut: Seq[Int], tangle: Tangle)
case class AnnularCobordism()

