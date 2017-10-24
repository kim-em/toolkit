package net.tqft.toolkit.algebra.grouptheory

/**
 * @author scott
 */
object IndependentSetsApp extends App {
  for (k <- 0 until 5) {
    val I = IndependentSets(k)
    val E = I.IndependentSet(Set.empty)
    println(s"k = $k")
//    for(s <- E.runtimeEstimatorStrings.take(10)) println(s)
    println("# of independent sets = " + E.descendants().size)
   
    for(s <-  E.descendants()) {
      require(s.verifyInverses)
      println(s)
    }
  }
}