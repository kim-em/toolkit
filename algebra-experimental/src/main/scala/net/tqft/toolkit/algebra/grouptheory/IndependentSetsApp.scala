package net.tqft.toolkit.algebra.grouptheory

/**
 * @author scott
 */
object IndependentSetsApp extends App {
  for (k <- 0 until 10) {
    val I = IndependentSets(k)
    val E = I.IndependentSet(Set.empty)
    println(s"k = $k")
//    for(s <- E.runtimeEstimatorStrings.take(10)) println(s)
    println("# of independent sets = " + E.descendants().size)
  }
}