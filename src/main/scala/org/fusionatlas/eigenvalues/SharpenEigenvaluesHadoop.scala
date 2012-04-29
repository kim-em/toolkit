package org.fusionatlas.eigenvalues

import net.tqft.toolkit.hadoop.HadoopSeq
import net.tqft.toolkit.hadoop.HadoopMatrix
import net.tqft.toolkit.algebra.Matrix

object SharpenEigenvaluesHadoop extends App {
  val (target, bounds) = args(0) match {
    case "2" => (Hadamard12.transfer2, (19.9, 20.2))
    case "3" => (Hadamard12.transfer3, (59.9, 60.1))
    case "4" => (Hadamard12.transfer4, (353.95, 352.97))
  }
    val hadoopTarget = new HadoopMatrix(target.numberOfColumns, HadoopSeq.from(target.entries))
    println(SharpenEigenvalues.findEigenvalue(hadoopTarget, bounds, 100))
}