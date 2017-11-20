package net.tqft.toolkit.algebra.mtc

import java.io.File
import net.tqft.toolkit.algebra.graphs.Dreadnaut
import net.tqft.toolkit.algebra.graphs.MatricesUpToSimultaneousRowAndColumnPermutations
import net.tqft.toolkit.arithmetic.Factor

object ModularDataAutomorphism extends App {
  def automorphismGroup(file: File) = {
    Dreadnaut.automorphismGroup(MatricesUpToSimultaneousRowAndColumnPermutations.convertToGraph(ModularDataLoader.superimposedST(file)))
  }

  for (n <- 27 to 48) {
    for (j <- 1 to 1000; dir = new File("../modular-data/code/Modular_Data/" + n + "/" + j); if dir.exists) {
      for (k <- 1 to 1000; file = new File("../modular-data/code/Modular_Data/" + n + "/" + j + "/" + k + ".txt"); if file.exists) {
        println(n + " " + j + " " + k)
        println(automorphismGroup(file).size + " = " + Factor(automorphismGroup(file).size))
      }
    }
  }

}