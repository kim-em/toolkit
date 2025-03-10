package net.tqft.toolkit.algebra.grouptheory

import scala.language.reflectiveCalls

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra.categories.Homomorphism

object FiniteGroups {

  def trivialGroup[A](one: A): FiniteGroup[A] = new TrivialGroup(one)
  def cyclicGroup(n: Int): FiniteGroup[Int] = new CyclicGroup(n)
  def dihedralGroup(n: Int): FiniteGroup[(Int, Boolean)] = {
    if (n == 0) {
      new TrivialGroup((0, false))
    } else {
      new DihedralGroup(n)
    }
  }

  trait LeftCoset[A] extends EquivalenceClass[A] {
    def group: FiniteGroup[A]
    def stabilizer: FiniteGroup[A]

    override def contains(a: A) = stabilizer.elements.contains(group.multiply(group.inverse(a), representative))

    def elements = (stabilizer.elements.map { group.multiply(representative, _) })
    override def toString = elements.mkString("LeftCoset[", ", ", "]")
  }

  trait DoubleCoset[A] extends EquivalenceClass[A] {
    def group: FiniteGroup[A]
    def leftStabilizer: FiniteGroup[A]
    def rightStabilizer: FiniteGroup[A]

    // TODO is there a better implementation of contains than the one inherited from EquivalenceClass? 
    // def contains(a: A) = elements.contains(a)

    def elements = for (h <- leftStabilizer.elements; g <- rightStabilizer.elements; x = representative) yield group.multiply(h, group.multiply(x, g))
    override def toString = elements.mkString("DoubleCoset[", ", ", "]")
  }

  def leftCosets[A](_group: FiniteGroup[A], subgroup: FiniteGroup[A]): Set[LeftCoset[A]] = {
    class C(val representative: A) extends LeftCoset[A] {
      def group = _group
      def stabilizer = subgroup
    }

    def extractCosets(elements: Set[A], cosets: Set[LeftCoset[A]]): Set[LeftCoset[A]] = {
      if (elements.isEmpty) {
        cosets
      } else {
        val newCoset = new C(elements.head)
        extractCosets(elements -- newCoset.elements, cosets + newCoset)
      }
    }
    val result = extractCosets(_group.elements, Set())
    result ensuring { _.size == _group.elements.size / subgroup.elements.size }
  }

  def doubleCosets[A](_group: FiniteGroup[A], leftSubgroup: FiniteGroup[A], rightSubgroup: FiniteGroup[A]): Set[DoubleCoset[A]] = {
    class D(val representative: A) extends DoubleCoset[A] {
      def group = _group
      def leftStabilizer = leftSubgroup
      def rightStabilizer = rightSubgroup
    }

    def extractCosets(elements: Set[A], cosets: Set[DoubleCoset[A]]): Set[DoubleCoset[A]] = {
      if (elements.isEmpty) {
        cosets
      } else {
        val newCoset = new D(elements.head)
        extractCosets(elements -- newCoset.elements, cosets + newCoset)
      }
    }
    extractCosets(_group.elements, Set())
  }

  def quotient[A](_group: FiniteGroup[A], normalSubgroup: FiniteGroup[A]): FiniteGroup[LeftCoset[A]] = {
    class C(val representative: A) extends LeftCoset[A] {
      def group = _group
      def stabilizer = normalSubgroup
    }

    new FiniteGroup[LeftCoset[A]] {
      override def one = new C(_group.one)
      override def inverse(a: LeftCoset[A]) = new C(_group.inverse(a.representative))
      override def multiply(a: LeftCoset[A], b: LeftCoset[A]) = new C(_group.multiply(a.representative, b.representative))

      override def elements = leftCosets(_group, normalSubgroup)

    }
  }

  private class TrivialGroup[A](identity: A) extends FiniteGroup[A] {
    override def one = identity
    override def inverse(a: A) = identity
    override def multiply(a: A, b: A) = identity

    override val elements = Set(identity)

  }

  // this is the dihedral group with n elements
  private class DihedralGroup(n: Int) extends FiniteGroup[(Int, Boolean)] {
    require(n > 0 && n % 2 == 0)

    val k = n / 2
    import net.tqft.toolkit.arithmetic.Mod._

    override def one = (0, false)
    override def inverse(a: (Int, Boolean)) = (if (a._2) a._1 else (-a._1 mod k), a._2)
    override def multiply(a: (Int, Boolean), b: (Int, Boolean)) = ((a._1 + (if (a._2) -b._1 else b._1)) mod k, a._2 ^ b._2)

    override val elements = (for (j <- Set(false, true); i <- 0 until k) yield (i, j))

  }
  private class CyclicGroup(n: Int) extends FiniteGroup[Int] {
    import net.tqft.toolkit.arithmetic.Mod._

    override def one = 0
    override def inverse(a: Int) = -a mod n
    override def multiply(a: Int, b: Int) = (a + b) mod n

    override val elements = (0 until n).toSet
  }
  import net.tqft.toolkit.permutations.Permutations.Permutation
  class SymmetricGroup(n: Int) extends FiniteGroup[IndexedSeq[Int]] {
    import net.tqft.toolkit.permutations.Permutations
    import net.tqft.toolkit.permutations.Permutations._
    override def elements = Permutations.of(n).toSet
    override def inverse(x: IndexedSeq[Int]) = Permutations.inverse(x)
    override def multiply(x: IndexedSeq[Int], y: IndexedSeq[Int]) = x.permute(y)
    override def one = 0 until n

    def permutationAction[C] = new Action[Seq[C]] {
      override def act(a: IndexedSeq[Int], x: Seq[C]) = a permute x
    }
  }

  val symmetricGroup: Int => SymmetricGroup = {
    import net.tqft.toolkit.functions.Memo._
    { n: Int => new SymmetricGroup(n) }.memo
  }

  def signature(n: Int): FiniteGroupHomomorphism[IndexedSeq[Int], Int] = new FiniteGroupHomomorphism[IndexedSeq[Int], Int] {
    val source = symmetricGroup(n)
    val target = cyclicGroup(2)
    def apply(p: IndexedSeq[Int]) = {
      var k = 0
      for (i <- 0 until p.size; j <- 0 until i; if p(i) < p(j)) k = k + 1
      k % 2
    }
  }

  val alternatingGroup: Int => FiniteGroup[Permutation] = {
    import net.tqft.toolkit.functions.Memo._
    { n: Int => signature(n).kernel }.memo
  }
  val signedPermutationGroup: Int => FiniteGroup[(Seq[Int], Permutation)] = {
    import net.tqft.toolkit.functions.Memo._
    { k: Int =>
      {
        val sym = symmetricGroup(k)
        semidirectProduct(power(cyclicGroup(2), k), sym)(sym.permutationAction)
      }
    }.memo
  }

  lazy val Mathieu11 = symmetricGroup(11).subgroupGeneratedBy(Seq(
    IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0),
    IndexedSeq(0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7)))
  lazy val Mathieu12 = symmetricGroup(12).subgroupGeneratedBy(Seq(
    IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11),
    IndexedSeq(0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7, 11),
    IndexedSeq(11, 10, 5, 7, 8, 2, 9, 3, 4, 6, 1, 0)))
  lazy val Mathieu22 = symmetricGroup(22).subgroupGeneratedBy(Seq(
    IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 11),
    IndexedSeq(3, 7, 0, 4, 8, 1, 5, 9, 2, 6, 10, 14, 18, 11, 15, 19, 12, 16, 20, 13, 17, 21),
    IndexedSeq(20, 9, 12, 16, 18, 1, 6, 5, 17, 7, 21, 13, 3, 15, 14, 19, 2, 4, 8, 11, 0, 10)))
  lazy val Mathieu23 = symmetricGroup(23).subgroupGeneratedBy(Seq(
    IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 0),
    IndexedSeq(0, 1, 16, 12, 3, 5, 8, 17, 2, 6, 11, 22, 13, 18, 19, 14, 9, 10, 4, 21, 15, 20, 7)))
  lazy val Mathieu24 = symmetricGroup(24).subgroupGeneratedBy(Seq(
    IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 0, 23),
    IndexedSeq(0, 1, 16, 12, 3, 5, 8, 17, 2, 6, 11, 22, 13, 18, 19, 14, 9, 10, 4, 21, 15, 20, 7, 23),
    IndexedSeq(23, 22, 11, 15, 17, 9, 19, 13, 20, 5, 16, 2, 21, 7, 18, 3, 10, 4, 14, 6, 8, 12, 1, 0)))

  def semidirectProduct[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B])(action: group2.Action[A]): FiniteGroup[(A, B)] = {
    new FiniteGroup[(A, B)] {
      override def elements = for (g1 <- group1.elements; g2 <- group2.elements) yield (g1, g2)
      override def one = (group1.one, group2.one)
      override def inverse(x: (A, B)) = {
        val bInverse = group2.inverse(x._2)
        (action.act(bInverse, group1.inverse(x._1)), bInverse)
      }
      override def multiply(x: (A, B), y: (A, B)) = (group1.multiply(x._1, action.act(x._2, y._1)), group2.multiply(x._2, y._2))

    }
  }

  def product[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B]) = semidirectProduct(group1, group2)(group2.trivialAction)

  def indexedProduct[A](groups: Seq[FiniteGroup[A]]): FiniteGroup[Seq[A]] = {
    new FiniteGroup[Seq[A]] {
      override def elements = ???
      override def one = groups.map(_.one)
      override def inverse(x: Seq[A]) = groups.zip(x).map(p => p._1.inverse(p._2))
      override def multiply(x: Seq[A], y: Seq[A]) = groups.zip(x.zip(y)).map(p => p._1.multiply(p._2._1, p._2._2))
    }
  }

  def power[A](group: FiniteGroup[A], k: Int): FiniteGroup[Seq[A]] = {
    new FiniteGroup[Seq[A]] {
      override def elements = {
        def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
          xs.foldLeft(Seq(Seq.empty[A])) {
            (x, y) => for (a <- x.view; b <- y) yield a :+ b
          }

        combine(Seq.fill(k)(group.elements)).toSet
      }
      override def one = Seq.fill(k)(group.one)
      override def inverse(x: Seq[A]) = x.map(group.inverse _)
      override def multiply(x: Seq[A], y: Seq[A]) = (x zip y).map({ case (xg, yg) => group.multiply(xg, yg) })

    }
  }

//  trait `SL(n, GF(q))`[I] extends FinitelyGeneratedFiniteGroup[Seq[Seq[Polynomial[I]]]] {
//    ???
//  }
  trait `SL(n, Z/pZ)`[I] extends FinitelyGeneratedFiniteGroup[Seq[Seq[I]]] {
    def integers: IntegerModel[I]
    implicit lazy val ring = PrimeField(p)(integers)
    lazy val matrixRing = Matrices.matricesOver(n)(ring)
    def n: Int
    def p: I
    override def multiply(x: Seq[Seq[I]], y: Seq[Seq[I]]) = {
      matrixRing.multiply(Matrix(n, x), Matrix(n, y)).entries.seq
    }
    override lazy val one = Matrix.diagonalMatrix(Seq.fill(n)(ring.one)).entries.seq
    override def inverse(x: Seq[Seq[I]]) = Matrix(n, x).inverse.get.entries.seq
  }
  trait `SL(2, Z/pZ)`[I] extends `SL(n, Z/pZ)`[I] {
    override val n = 2
    def S: Seq[Seq[I]] = Seq(Seq(ring.zero, ring.negate(ring.one)), Seq(ring.one, ring.zero))
    def T: Seq[Seq[I]] = Seq(Seq(ring.one, ring.one), Seq(ring.zero, ring.one))
    override def generators = Seq(S, T)
  }

  def `SL(2, Z/pZ)`[I: IntegerModel](p: I) = {
    val p_ = p
    new `SL(2, Z/pZ)`[I] {
      override def p = p_
      override def integers = implicitly[IntegerModel[I]]
    }
  }
}

trait FiniteGroupHomomorphism[A, B] extends Homomorphism[FiniteGroup, A, B] { homomorphism =>
  def kernel = source.subgroup(source.elements.par.filter(homomorphism(_) == target.one).seq)
}

trait FinitelyGeneratedFiniteGroupHomomorphism[A, B] extends Homomorphism[FinitelyGeneratedFiniteGroup, A, B] {
  override def apply(p: A) = {
    target.product(source.elementsAsWordsInGenerators(p).map(applyToGenerator))
  }
  def applyToGenerator(x: Int): B
}

