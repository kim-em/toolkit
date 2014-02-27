package net.tqft.toolkit.algebra.matrices

import net.tqft.toolkit.algebra.ApproximateReals
import net.tqft.toolkit.algebra.IntegerModel
import net.tqft.toolkit.functions.FixedPoint
import com.google.common.cache.CacheLoader
import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit

object FrobeniusPerronEigenvalues {
  def estimate[I: IntegerModel, R: ApproximateReals](m: Matrix[I]): R = {
    val integers = implicitly[IntegerModel[I]]
    val reals = implicitly[ApproximateReals[R]]

    val mR = m.mapEntries[R](reals.fromInteger)
    val initialVector = reals.normalize(m.entries.map(v => reals.abs(reals.fromInteger(integers.add(integers.sum(v), integers.one)))).seq)
    val initialEstimate = reals.zero

    def next(p: (Seq[R], R)) = {
      val w = mR(p._1)
      val n = reals.norm(w)
      val nw = w.map(x => reals.quotient(x, n))
      (nw, n)
    }

    FixedPoint.withSameTest({ (p: (Seq[R], R), q: (Seq[R], R)) => reals.close(p._2, q._2) })(next)(initialVector, initialEstimate)._2
  }

  private val matrices = Matrices.over[Double]

  def estimate2(m: Matrix[Int]): Double = {
    //    require(m == m.transpose)

    val reals = implicitly[ApproximateReals[Double]]

    val mR = m.mapEntries(_.toDouble)
    val mR2 = matrices.compose(mR, mR)
    val iv = mR2.entries.map(v => v.sum + 1).seq
    val initialVector = reals.normalize(iv)
    val initialEstimate = reals.zero

    def next(p: (Seq[Double], Double)) = {
      val w = mR2(p._1)
      val n = reals.norm(w)
      val nw = w.map(x => reals.quotient(x, n))
      (nw, n)
    }

    scala.math.sqrt(FixedPoint.withSameTest({ (p: (Seq[Double], Double), q: (Seq[Double], Double)) => (p._2 - q._2).abs < 0.0001 })(next)(initialVector, initialEstimate)._2)
  }

  //  val estimateWithEigenvector = {
  //    import net.tqft.toolkit.functions.Memo
  //
  //    val seen = scala.collection.mutable.Set[Int]()
  //    var hits = 0
  //    var misses = 0;
  //    ({ m: Array[Array[Int]] =>
  //      val h = m.toSeq.map(_.toSeq).hashCode
  //      if (seen.contains(h)) {
  //        hits += 1
  //      } else {
  //        seen += h
  //        misses += 1
  //      }
  //      println("hits: " + hits + " misses: " + misses)
  //      estimateWithEigenvector_(m)
  //    })
  //  }

  val estimateWithEigenvector = {
    val loader =
      new CacheLoader[Array[Array[Int]], (Double, Seq[Double])]() {
        override def load(key: Array[Array[Int]]) = {
          estimateWithEigenvector_(key)
        }
      }

    val cache = CacheBuilder.newBuilder().maximumSize(20000)
//          .expireAfterAccess(5, TimeUnit.MINUTES)
          .build(loader)

          {  m: Array[Array[Int]] => cache.getUnchecked(m) }
    
  }

  val estimateWithEigenvector2 = {
    // FIXME replace this with a proper cache; limit the size directly, rather than using softValues.
    val cache = new com.google.common.collect.MapMaker().softValues().makeMap[Int, (Array[Array[Int]], (Double, Seq[Double]))]()
    def hash(m: Array[Array[Int]]): Int = {
      def hash(v: Array[Int]): Int = {
        v.foldLeft(1)(7 * _ + _)
      }
      m.foldLeft(1)(13 * _ + hash(_))
    }
    def eq(m: Array[Array[Int]], v: Array[Array[Int]]): Boolean = {
      m.length == v.length && {
        var i = m.length - 1
        var j = m.length - 1
        while (i != -1 && m(i)(j) == v(i)(j)) {
          j -= 1
          if (j < 0) {
            i -= 1
            j = m.length - 1
          }
        }
        i == -1
      }
    }
    var hits = 0
    var collisions = 0
    var misses = 0;
    { m: Array[Array[Int]] =>
      if ((hits + collisions + misses) % 100000 == 0) println("hits: " + hits + " collisions: " + collisions + " misses: " + misses + " cache.size: " + cache.size)
      val h = hash(m)
      if (cache.containsKey(h)) {
        val p = cache.get(h)
        if (eq(m, p._1)) {
          hits += 1
          p._2
        } else {
          collisions += 1
          //          println(m.toSeq.map(_.toSeq))
          //          println(p._1.toSeq.map(_.toSeq))
          //          println(hash(m))
          //          println(hash(p._1))
          //          require(false)
          val r = estimateWithEigenvector_(m)
          cache.put(h, (m.map(_.clone).clone, r))
          r
        }
      } else {
        misses += 1
        val r = estimateWithEigenvector_(m)
        cache.put(h, (m.map(_.clone).clone, r))
        r
      }
    }
  }

  def estimateWithEigenvector_(m: Array[Array[Int]] /*, hint: Option[Array[Double]] = None*/ ): (Double, Seq[Double]) = {
    val rank = m.length
    //    var iv0 = hint.getOrElse(m.map(_.sum + 1.0))
    var iv0 = m.map(_.sum + 1.0)
    var est1 = scala.math.sqrt(iv0.map(x => x * x).sum)
    for (i <- 0 until rank) {
      iv0(i) = iv0(i) / est1
    }
    var iv1 = Array.fill(rank)(0.0)
    var est0 = 0.0
    var k = 0
    //    var count = 0
    while (scala.math.abs(est1 - est0) > 0.0001) {
      est0 = est1
      est1 = 0.0
      k = 0
      while (k < rank) {
        var j = 0
        iv1(k) = 0.0
        while (j < rank) {
          iv1(k) += m(k)(j) * iv0(j)
          j += 1
        }
        est1 += iv1(k) * iv1(k)
        k += 1
      }
      est1 = scala.math.sqrt(est1)
      k = 0
      while (k < rank) {
        iv0(k) = iv1(k) / est1
        k += 1
      }
    }

    (est1, iv0)
  }
}