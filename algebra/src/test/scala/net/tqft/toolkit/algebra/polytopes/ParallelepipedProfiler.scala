package net.tqft.toolkit.algebra.polytopes

import net.tqft.toolkit.Profiler

object ParallelepipedProfiler extends App {
  // 355262ms with BigInteger arithmetic
  // 115888ms with Double arithmetic, falling back to BigInteger arithmetic if there's any doubt
  // 118960ms with separator iterators for the interior and boundary, only considering all roundings on the boundary
    
  val p = Parallelepiped(Seq(Seq(-6, 3, 3, 2, 2), Seq(1, 6, 14, -12, 43), Seq(10, -8, 10, -7, 12), Seq(11, -6, 3, 2, 7), Seq(1, 0, 0, 0, 0)))
  val scaleFactors = Seq(0.0862163, 0.0159058, 0.0211167, 0.0291209, 0.500846)
  println(p.volume)
  val (time, size) = Profiler.timing(p.enumeratePointsUsingMesh(scaleFactors).size)
  println(time)
  println(size)

}