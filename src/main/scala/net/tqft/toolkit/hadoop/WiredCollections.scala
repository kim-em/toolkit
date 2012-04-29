package net.tqft.toolkit.hadoop

import com.nicta.scoobi.Scoobi.WireFormat
import scala.collection.generic.CanBuildFrom
import scala.collection.GenSeq

// provides better interoperability between Scala collections and HadoopSeqs.

object WiredCollections {
  implicit def mappableAsWiredMappable[CC[X] <: GenSeq[X], A](c: CC[A]): WiredMappable[CC, A] = new WiredMappable(c)

  class WiredMappable[CC[X] <: GenSeq[X], A](c: CC[A]) {
    def wireMap[B](f: A => B)(implicit bManifest: Manifest[B], bWireFormat: WireFormat[B], cbf: CanBuildFrom[GenSeq[A], B, CC[B]]): CC[B] = {
      if (c.isInstanceOf[HadoopSeq[_]]) {
        c.asInstanceOf[HadoopSeq[A]].wireMap(f).asInstanceOf[CC[B]]
      } else {
        c.map[B, CC[B]](f)
      }
    }
    def wireSortBy[B: Manifest: WireFormat: Ordering](f: A => B): CC[A] = {
      if (c.isInstanceOf[HadoopSeq[_]]) {
        c.asInstanceOf[HadoopSeq[A]].wireSortBy(f).asInstanceOf[CC[A]]
      } else {
        c.seq.sortBy(f).asInstanceOf[CC[A]]
      }
    }
    def invariantMap(f: A => A)(implicit cbf: CanBuildFrom[GenSeq[A], A, CC[A]]): CC[A] = {
      if (c.isInstanceOf[HadoopSeq[_]]) {
        c.asInstanceOf[HadoopSeq[A]].invariantMap(f).asInstanceOf[CC[A]]
      } else {
        c.map[A, CC[A]](f)
      }
    }
    def newBuilder = {
      if (c.isInstanceOf[HadoopSeq[_]]) {
        HadoopSeq.newWiredBuilder[A](c.asInstanceOf[HadoopSeq[A]].manifest, c.asInstanceOf[HadoopSeq[A]].wireFormat)
      } else {
        c.genericBuilder[A]
      }
    }
  }
}