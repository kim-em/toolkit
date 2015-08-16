package net.tqft.toolkit.algebra.khovanov

import scala.language.implicitConversions

case class Crossing(label: Int, sign: Int, strands: (Int, Int, Int, Int))
case class Arc(labels: Int*)
trait Loop {
  def labels: Seq[Int]
}
object Loop {
  def apply(labels: Int*) = {
    val labels_ = labels
    val p = labels.indexOf(labels.min)
    new Loop {
      override val labels = labels_.drop(p) ++ labels_.take(p)
    }
  }
}
case class Tangle(crossings: Seq[Crossing], arcs: Seq[Arc] = Seq.empty, loops: Seq[Loop] = Seq.empty)
case class Cobordism(source: Tangle, target: Tangle, frames: Seq[ElementaryCobordism]) {
  // convenience methods for building Cobordisms --- these all append a frame, and can look at the current target to fill in the details
  def createLoop(loop: Loop): Cobordism = ???
  def destroyLoop(loop: Loop): Cobordism = ???
  def destroyLoop(label: Int): Cobordism = ???
  def untwist(labelOnLoop: Int): Cobordism = ???
  // ...  
}

// ugh --- there are also just global renames, e.g. to achieve a 1-click rotation of a trefoil
// ugh --- there are also inserting and deleting labels
sealed trait ElementaryCobordism {
  def source: Tangle
  def target: Tangle
}
case class CreateLoop(loop: Loop) extends ElementaryCobordism {
  override def source = Tangle(Seq.empty)
  override def target = Tangle(Seq.empty, Seq.empty, Seq(loop))
}
case class DestroyLoop(loop: Loop) extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}
case class Saddle(se: Int, ne: Int, nw: Int, sw: Int) extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}

// TODO these aren't quite right, from here on:
case class Twist(incoming: Int, loop: Int, outgoing: Int) extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}
case class Untwist(incoming: Int, loop: Int, outgoing: Int) extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}
case class R2() extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}
case class R3() extends ElementaryCobordism {
  override def source = ???
  override def target = ???
}

object MovieMoves {
  // for now, let's just pick a representative of each one, and not worry about all the variants
  def all = MM1 #:: MM2 #:: MM3 #:: MM4 #:: MM5 #:: MM6 #:: MM7 #:: MM8 #:: MM9 #:: MM10 #:: MM11 #:: MM12 #:: MM13 #:: MM14 #:: MM15 #:: Stream.empty[(Cobordism, Cobordism)]
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
      require(x.target == y.source) // perhaps we should make this more general, like we do for cans? we'd have to write something for finding complements of tangles
      Cobordism(x.source, y.target, x.frames ++ y.frames)
    }
    def identity(o: Tangle): Cobordism = Cobordism(o, o, Nil)
    def source(x: Cobordism): Tangle = x.source
    def target(x: Cobordism): Tangle = x.target

    def tensorMorphisms(x: Cobordism, y: Cobordism): Cobordism = ???
    def tensorObjects(x: Tangle, y: Tangle): Tangle = ???
  }
}

// just a reminder for later
case class AnnularTangle(cut: Seq[Int], tangle: Tangle)
case class AnnularCobordism()

