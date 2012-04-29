package net.tqft.toolkit.hadoop

import net.tqft.toolkit.algebra.Matrix
import com.nicta.scoobi.Scoobi.WireFormat
import scala.collection.GenSeq

object HadoopMatrix {
  def apply[B: Manifest: WireFormat](numberOfColumns: Int, entries: GenSeq[Seq[B]]): HadoopMatrix[B] = new HadoopMatrix(numberOfColumns, HadoopSeq.from(entries))
}

class HadoopMatrix[B: Manifest: WireFormat](numberOfColumns: Int, override val entries: HadoopSeq[Seq[B]]) extends Matrix(numberOfColumns, entries) {

  override def joinRows(other: Matrix[B]) = {
    val zipped: HadoopSeq[(Seq[B], Seq[B])] = other match {
      case other: HadoopMatrix[_] => entries wireZip other.entries
      case _ => entries.wireZip(other.entries.seq)
    }
    new HadoopMatrix(numberOfColumns + other.numberOfColumns, zipped wireMap { case (r1, r2) => r1 ++ r2 })
  }

  //   def mapIndexedRows[A](rowMapper: ((Seq[B], Int)) => Seq[A], newRowSize: Int = numberOfColumns) = {
  //    val zipped = entries.zipWithIndex[Seq[B], HadoopSeq[Seq[B]]]
  //    new HadoopMatrix(newRowSize, zipped)//.wireMap(rowMapper))
  //  }
}