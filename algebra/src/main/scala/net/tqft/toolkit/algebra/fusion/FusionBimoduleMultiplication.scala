package net.tqft.toolkit.algebra.fusion

trait FusionBimoduleMultiplication {
  def left: FusionBimoduleWithDimensions
  def right: FusionBimoduleWithDimensions
  def target: FusionBimoduleWithDimensions

  case class PartialMultiplication(p: Int, q: Int, v: List[List[List[Int]]]) {
//    def conditionA = {
//      for (i <- 0 until p; j <- 0 until q) yield {
//      }
//    }
  }
}

