package net.tqft.toolkit.algebra.fusion

object AncestryTest extends App {

//  {
//    val ring = PartialFusionRingEnumeration(4, 0).PartialFusionRing("4,0 3 322221211221201111211111110 59.92")
//    println(ring.verifyStrictAncestry)
//    println(ring.verifyStrictAncestryForSomeIsomorph)
//  }

  {
    val enumeration = PartialFusionRingEnumeration(5, 0)
    val root = enumeration.root
    val ring = enumeration.PartialFusionRing("5,0 1 1001011001111010011011001011001101111011111111101010001111100101 30.1421")

    //    for (r <- ring.isomorphs) println(r.toShortString)
    //
//        println(ring.verifyAncestry)
        println(ring.verifyStrictAncestryForSomeIsomorph)
    //    for (r <- ring.ancestry) {
    //      println(r.toShortString)
    //      require(r.verifyInverses)
    //      require(r.verifyUpperOrbits)
    //    }

//    for (iso <- Some(ring)) {
//      println("isomorph: " + iso.toShortString)
//      val ancestry = iso.ancestry.toSeq
//
//      for(a <- ancestry) println(a.toShortString)
//      
//      for (d <- root.descendants({ r => if (ancestry.exists(s => s.isomorphicTo_?(r))) 1 else -1 })) {
//        println(d.toShortString + " <----> " + ancestry.find(s => s.isomorphicTo_?(d)).map(_.toShortString))
//      }
//    }

//    val missing = enumeration.PartialFusionRing("5,0 1 _00_0__00_1__0_00__0__00_01_00__0_1__01_111____0_0_000_____00_0_ 30.14")
//    
//    val t = enumeration.PartialFusionRing("5,0 1 _0_00_0__00_0___0_0___0000___0_1_00_00__0__0__0_0____0_1__0__1_1 30.14")
//    for(u <- t.upperObjects.elements) {
//      println(u + " " + u.result.toShortString + " " + u.result.isomorphicTo_?(missing))
//    }
//    
//    
//    println("children of " + t.toShortString)
//    for(c <- t.children) {
//      println(c.toShortString)
//    }
  }
}