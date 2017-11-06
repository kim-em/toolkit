package net.tqft.toolkit.arithmetic

/**
 * @author scott
 */
object Sum {
  def apply(i: Int)(f: Int => Int) = {
    var s = 0
    for(j <- 0 until i) s = s + f(j)
    s
  }
//  def apply(i: Int)(f: Int => Double) = {
//    var s = 0.0
//    for(j <- 0 until i) s = s + f(j)
//    s
//  }
}