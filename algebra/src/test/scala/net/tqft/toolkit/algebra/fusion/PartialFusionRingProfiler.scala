package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.Profiler
import net.tqft.toolkit.algebra.matrices.Matrix
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.WrappedArray

object PartialFusionRingProfiler extends App {

  val slowpfr= PartialFusionRing(2,FusionRing(IndexedSeq(Matrix(7, ArrayBuffer(Vector(1, 0, 0, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 0, 1, 0, 0, 0, 0), Vector(0, 0, 0, 1, 0, 0, 0), Vector(0, 0, 0, 0, 1, 0, 0), Vector(0, 0, 0, 0, 0, 1, 0), Vector(0, 0, 0, 0, 0, 0, 1))), Matrix(7, ArrayBuffer(Vector(0, 1, 0, 0, 0, 0, 0), Vector(1, 0, 1, 1, 1, 1, 1), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0))), Matrix(7, ArrayBuffer(Vector(0, 0, 1, 0, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 0, 0, 1, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 1), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0))), Matrix(7, ArrayBuffer(Vector(0, 0, 0, 1, 0, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0, 0), Vector(0, 0, 1, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 1, 0, 0))), Matrix(7, ArrayBuffer(Vector(0, 0, 0, 0, 1, 0, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 1), Vector(1, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 1, 0, 0, 0))), Matrix(7, ArrayBuffer(Vector(0, 0, 0, 0, 0, 1, 0), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0))), Matrix(7, ArrayBuffer(Vector(0, 0, 0, 0, 0, 0, 1), Vector(0, 1, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 1, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 1, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0, 0))))),12.0)
  println(slowpfr.children)
}