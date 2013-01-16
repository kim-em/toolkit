package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.permutations._

object Representations {
  private def elementaryVector[A](entry: A, zero: A, n: Int, k: Int) = Seq.fill(k)(zero) ++ (entry +: Seq.fill(n - k - 1)(zero))
  private def unitVector[A: Zero: One](n: Int, k: Int) = elementaryVector(implicitly[One[A]].one, implicitly[Zero[A]].zero, n, k)

  def permutationRepresentation[F: Ring](n: Int): Representation[Permutation, F] = permutationRepresentation(FiniteGroups.symmetricGroup(n))
  def permutationRepresentation[F: Ring](_source: FiniteGroup[Permutation]): Representation[Permutation, F] = {
    new Representation[Permutation, F] {
      override val degree = _source.one.size
      override val source = _source
      override def apply(p: Permutation) = {
        import net.tqft.toolkit.collections.SparseSeq
        Matrix(degree, for (k <- p) yield {
          SparseSeq.elementaryVector[F](degree, k, implicitly[Ring[F]].one, implicitly[Ring[F]].zero)
        })
      }
    }
  }
  def signedPermutationRepresentation[F: Ring](n: Int): Representation[(Seq[Int], Permutation), F] = signedPermutationRepresentation(FiniteGroups.signedPermutationGroup(n))
  def signedPermutationRepresentation[F: Ring](source: FiniteGroup[(Seq[Int], Permutation)]): Representation[(Seq[Int], Permutation), F] = {
    val _source = source
    new Representation[(Seq[Int], Permutation), F] {
      override val source = _source
      override val degree = _source.one._1.size
      override def apply(p: (Seq[Int], Permutation)) = {
        import net.tqft.toolkit.collections.SparseSeq
        val ring = implicitly[Ring[F]]
        import net.tqft.toolkit.collections.Pairs._
        Matrix(degree, for ((s, k) <- p.transpose) yield {
          SparseSeq.elementaryVector[F](degree, k, if (s == 0) ring.one else ring.fromInt(-1), ring.zero)
        })
      }
    }
  }

  def tensor[A, F: Ring](V: Representation[A, F], W: Representation[A, F]): Representation[A, F] = {
    require(V.source == W.source)
    new Representation[A, F] {
      override val source = V.source
      override def degree = V.degree * W.degree
      override def apply(a: A) = Matrices.tensor(V(a), W(a))
    }
  }

  def tensorPower[A, F: Ring](V: Representation[A, F], k: Int): Representation[A, F] = {
    new Representation[A, F] {
      override val source = V.source
      override def degree = Integers.power(V.degree, k)
      override def apply(a: A) = {
        val Va = V(a)
        Seq.fill(k)(Va).reduce(Matrices.tensor(_, _))
      }
    }
  }
}