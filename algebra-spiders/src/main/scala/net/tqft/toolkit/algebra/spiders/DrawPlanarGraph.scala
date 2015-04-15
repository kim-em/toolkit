package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders._

object DrawPlanarGraph {  
  private class Node(val label: Int, val x: Int, val y: Int, val valence: Int, val neighborRadius: Int) {
    def bearingTo(n: Node): Double = {
      0 //placeholder
    }
  }
  
  
  
  def apply(G: PlanarGraph, initialRadius: Int, decreaseRate: Int): String = {
    "placeholder"
  }
}