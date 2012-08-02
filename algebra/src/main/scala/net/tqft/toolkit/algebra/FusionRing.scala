package net.tqft.toolkit.algebra

// this is just scratch work for now!

trait FusionRing[A] extends Rig[A] {
  def basis: Seq[A]
  
  def candidateBrauerPicardGroupoids: Seq[Groupoid] = {
    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule])] = ???
    ???
  }
  
  trait FusionModule
  trait FusionBimodule
}

trait Groupoid

object FusionRing {
  def fromStructureCoefficients(rank: Int, m: (Int, Int) => Seq[Int]): FusionRing[Seq[Int]] = new StructureCoefficientFusionRing(rank, m)
  
  private class FreeRigModule[A:Rig](rank: Int) extends CommutativeMonoid[Seq[A]] {
    val zero = Seq.fill(rank)(implicitly[Rig[A]].zero)
    def add(x: Seq[A], y: Seq[A]) = x.zip(y).map(p => implicitly[Rig[A]].add(p._1, p._2))
  }
  
  import Implicits.Integers
  private class StructureCoefficientFusionRing(rank: Int, multiplicity: (Int, Int) => Seq[Int]) extends FreeRigModule[Int](rank) with FusionRing[Seq[Int]] {
    def basis = for(i <- 0 to rank) yield for(j <- 0 to rank) yield if(i == j) 1 else 0
    
    def fromInt(x: Int) = x +: Seq.fill(rank - 1)(0)
    val one = fromInt(1)
    def multiply(x: Seq[Int], y: Seq[Int]) = {
      for((xi, i) <- x.zipWithIndex; (yj, j) <- y.zipWithIndex) yield ???
    }
  }
}

trait IndeterminateFusionRing extends FusionRing[Option[Int]] {
  
}

object Goals {
  val AH: FusionRing[Int] = ???
  AH.candidateBrauerPicardGroupoids 
}