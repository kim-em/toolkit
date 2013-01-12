package net.tqft.toolkit.algebra.graphs

object dreadnautTest extends App {
  val g = Graph(3, Seq(Set(0,1), Set(1,2), Set(2,0)))
  println(dreadnaut.automorphismGroup(g).generators)
  println(dreadnaut.canonicalize(g).edges)
}