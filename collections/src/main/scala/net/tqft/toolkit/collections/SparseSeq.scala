package net.tqft.toolkit.collections

import scala.language.implicitConversions

import scala.collection.SeqLike
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer

trait SparseSeq[A] extends Seq[A] with SeqLike[A, SparseSeq[A]] { sparseSeq =>
  def zero: A
  def firstNonZeroEntry: Option[(A, Int)] = nonZeroEntries.headOption
  def nonZeroEntries: Seq[(A, Int)]

  override def apply(i: Int) = nonZeroEntries.find(_._2 == i).map(_._1).getOrElse(zero)
  override def iterator: Iterator[A] = new Iterator[A] {
    var k = 0
    var j = 0
    def hasNext = k < sparseSeq.length
    def next: A = {
      if (j >= nonZeroEntries.size || k < nonZeroEntries(j)._2) {
        k = k + 1
        zero
      } else {
        val result = nonZeroEntries(j)._1
        k = k + 1
        j = j + 1
        result
      }
    }
  }
  override def newBuilder: Builder[A, SparseSeq[A]] = new SparseSeq.SparseSeqBuilder(zero)
  override def seq = this
}

object SparseSeq {
  implicit def numericSeq2SparseSeq[T: Numeric](s: Seq[T]): SparseSeq[T] = {
    val zero = implicitly[Numeric[T]].zero
    new SparseSeqSeqImpl(s, zero)
  }

  def from[T](s: Seq[T], zero: T): SparseSeq[T] = {
    if (s.isInstanceOf[SparseSeq[_]] && s.asInstanceOf[SparseSeq[T]].zero == zero) {
      s.asInstanceOf[SparseSeq[T]]
    } else {
      new SparseSeqSeqImpl(s, zero)
    }
  }

  def withEntries[T](size: Int, entries: Seq[(T, Int)], zero: T): SparseSeq[T] = new SparseSeqSeqImpl(size, entries.sortBy(_._2), zero)

  def unitVector[T: Numeric](n: Int, k: Int): SparseSeq[T] = elementaryVector(n, k, implicitly[Numeric[T]].one)

  def elementaryVector[T](n: Int, k: Int, value: T, zero: T): SparseSeq[T] = new SparseSeqSeqImpl(n, List((value, k)), zero)
  def elementaryVector[T: Numeric](n: Int, k: Int, value: T): SparseSeq[T] = elementaryVector(n, k, value, implicitly[Numeric[T]].zero)

  private class SparseSeqBuilder[A](zero: A) extends Builder[A, SparseSeq[A]] {
    val internal = new ListBuffer[(A, Int)]
    var s = 0
    override def clear {
      internal.clear
      s = 0
    }
    override def result: SparseSeq[A] = new SparseSeqSeqImpl(s, internal.result, zero)
    override def +=(a: A) = {
      if (a != zero) {
        internal += ((a, s))
      }
      s = s + 1
      this
    }
  }

  private trait SparseSeqImpl[A] extends SparseSeq[A]
  private class SparseSeqSeqImpl[A](override val length: Int, override val nonZeroEntries: Seq[(A, Int)], override val zero: A) extends SparseSeqImpl[A] {
    def this(s: Seq[A], zero: A) = this(s.size, s.zipWithIndex.filter(_._1 != zero), zero)
  }
}