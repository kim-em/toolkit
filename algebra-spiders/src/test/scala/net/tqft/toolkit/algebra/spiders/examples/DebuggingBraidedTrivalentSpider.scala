package net.tqft.toolkit.algebra.spiders.examples

import net.tqft.toolkit.algebra.spiders._

object DebuggingBraidedTrivalentSpider extends App {
  val bts = BraidedTrivalentSpider
  val one = bts.ring.one
//  println(bts.canonicalForm(Map(PlanarGraph.hopfStrand -> one)))
  println(
      BraidedTrivalentSpider.innerProduct(Map(PlanarGraph.hopfStrand -> one), Map(PlanarGraph.hopfStrand -> one))
      )
}