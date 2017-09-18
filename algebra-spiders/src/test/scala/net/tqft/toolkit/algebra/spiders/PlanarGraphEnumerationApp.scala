package net.tqft.toolkit.algebra.spiders

object PlanarGraphEnumerationApp extends App {
  val context = PlanarGraphEnumerationContext(Seq(VertexType(3,1,1)))
  val root = context.PlanarGraphEnumeration(PlanarGraph.polygon(3))
  println(root.G)
  println("lower objects")
  for(l <- root.lowerObjects.elements) println(l)
  println("preferred lower object")
  println(root.lowerObjects.allOrbits.min(root.ordering).representative)
  
  //  val context = PlanarGraphEnumerationContext(Seq(VertexType(3,1,1)))
//  val root = context.PlanarGraphEnumeration(PlanarGraph.polygon(5))
//  for(child<-root.children) {
//    DrawPlanarGraph.showPDF(child.G)
//  }
//  val context = PlanarGraphEnumerationContext(Seq(VertexType(3,1,1)))
//  val root = context.PlanarGraphEnumeration(PlanarGraph.star(3,1,1))
//  for(child<-root.children) {
//    DrawPlanarGraph.showPDF(child.G)
//  }
}

