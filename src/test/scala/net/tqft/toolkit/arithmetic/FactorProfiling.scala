package net.tqft.toolkit.arithmetic

import net.tqft.toolkit.Profiler

object FactorProfiling extends App {

  while (true) {
    println(Profiler.timing(
      ecm.Factor(BigInt("3284464815846549089237979195192428016693977093"))))
  }

}
 