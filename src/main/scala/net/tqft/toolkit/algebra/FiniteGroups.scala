package net.tqft.toolkit.algebra

import scala.collection.immutable.TreeSet
import scala.collection.immutable.HashSet
import scala.collection.GenSet

trait FiniteGroup[A] extends Group[A] with Elements[A] { finiteGroup =>
  implicit def ordering: Ordering[A]

  def verifyInverses = {
    for (x <- elements) {
      require(multiply(x, inverse(x)) == one)
      require(multiply(inverse(x), x) == one)
    }
    true
  }

  def verifyAssociativity = {
    for (x <- elements; y <- elements; z <- elements) {
      require(multiply(multiply(x, y), z) == multiply(x, multiply(y, z)))
    }
    true
  }

  def verifySubgroup(subgroup: Set[A]) = {
    for (x <- subgroup) {
      require(subgroup.contains(inverse(x)))
    }
    for (x <- subgroup; y <- subgroup) {
      require(subgroup.contains(multiply(x, y)))
    }
    true
  }

  def verifyNormalSubgroup(subgroup: Set[A]) = {
    verifySubgroup(subgroup)
    for (x <- subgroup; y <- elements) {
      val p = multiply(multiply(inverse(y), x), y)
      require(subgroup.contains(p))
    }
    true
  }

  private class Subgroup(val elements: Set[A]) extends FiniteGroup[A] {
    def one = finiteGroup.one
    def inverse(a: A) = finiteGroup.inverse(a)
    def multiply(a: A, b: A) = finiteGroup.multiply(a, b)
    def ordering = finiteGroup.ordering
  }
  private class FinitelyGeneratedSubgroup(val generators: Set[A]) extends Subgroup({
    import net.tqft.toolkit.functions.FixedPoint
    FixedPoint({ s: GenSet[A] => s flatMap { x => generators map { multiply(_, x) } } })(Set(one).par).seq.toSet
  }) with FinitelyGeneratedFiniteGroup[A]

  def subgroup(elements: Set[A]): FiniteGroup[A] = new Subgroup(elements)
  def subgroupGeneratedBy(generators: Set[A]): FinitelyGeneratedFiniteGroup[A] = new FinitelyGeneratedSubgroup(generators)

  protected def unsortedConjugacyClasses = GroupActions.conjugationAction(finiteGroup).orbits(elements, elements).toSeq

  lazy val conjugacyClasses = unsortedConjugacyClasses.sortBy({ c => (c.representative != one, c.elements.size) })
  lazy val conjugacyClassOrders = for (c <- conjugacyClasses) yield orderOfElement(c.representative)
  val exponentiationOnConjugacyClasses: Int => Seq[Int] = {
    def impl(k: Int) = conjugacyClasses.map({ cx => conjugacyClasses.indexWhere({ cy => cy.elements.contains(power(cx.representative, k)) }) })
    import net.tqft.toolkit.functions.Memo._
    (impl _).memo
  }
  def inverseConjugacyClasses = {
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation
    exponentiationOnConjugacyClasses(-1) permute conjugacyClasses
  }

  lazy val classCoefficients: Seq[Seq[Seq[Int]]] = {
    for ((cx, ix) <- inverseConjugacyClasses.zipWithIndex) yield {
      (for ((cy, iy) <- conjugacyClasses.zipWithIndex.par) yield {
        println((ix, iy))
        for (cz <- conjugacyClasses; z = cz.representative) yield {
          cx.elements.count({ x => cy.elements.contains(multiply(x, z)) })
        }
      }).seq
    }
  }

  def orderOfElement(a: A): Int = Iterator.iterate(a)(multiply(_, a)).indexOf(one) + 1
  lazy val exponent = Gadgets.Integers.lcm((elements map { orderOfElement _ }).toSeq: _*)

  lazy val preferredPrime = {
    var p = exponent + 1
    while (!BigInt(p).isProbablePrime(60)) p = p + exponent
    p
  }

  lazy val characterTable: (Int, Seq[Seq[Polynomial[Fraction[Int]]]]) = {
    def classCoefficientSimultaneousEigenvectorsModPrime = {
      import net.tqft.toolkit.arithmetic.Mod._
      implicit val modP = Mod(preferredPrime)
      val zeroVector = List.fill(conjugacyClasses.size)(0).toSeq

      def subtractDiagonal(m: Seq[Seq[Int]], lambda: Int) = {
        m.zipWithIndex.map({ case (r, i) => r.updated(i, (r(i) - lambda) mod preferredPrime) })
      }

      def eigenvalues(m: Seq[Seq[Int]]) = new Matrix(m.size, m).eigenvalues

      case class PartialEigenspace(annihilators: Seq[Seq[Seq[Int]]], eigenvalues: Seq[Int], eigenvectors: Option[Seq[Seq[Int]]]) {
        def splitAlong(m: Seq[Seq[Int]], mEigenvalues: Set[Int]): Set[PartialEigenspace] = {
          eigenvectors.map(_.size) match {
            case Some(0) => Set()
            case Some(1) => {
              Set(this)
            }
            case _ => {
              for (lambda <- mEigenvalues) yield {
                val newAnnihilators = annihilators :+ subtractDiagonal(m, lambda)
                PartialEigenspace(newAnnihilators, eigenvalues :+ lambda, Some(new Matrix(conjugacyClasses.size, newAnnihilators.flatten.toSeq).nullSpace))
              }
            }
          }
        }
      }

      val classCoefficientsWithEigenvalues = classCoefficients.tail.par.map({ cc => (cc, eigenvalues(cc)) }).seq
      val unnormalizedEigenvectors = classCoefficientsWithEigenvalues.foldLeft(Set(PartialEigenspace(Seq(), Seq(), None)).par)({ case (s, (cc, ev)) => s.flatMap({ p => p.splitAlong(cc, ev) }) }).flatMap(_.eigenvectors.get).toSeq
      val normalizedEigenvectors = unnormalizedEigenvectors.map({ v => v.map({ x => modP.quotient(x, v(0)) }) })

      //      def proportional(w: Seq[Int], v: Seq[Int]) = {
      //        if (v == zeroVector) {
      //          w == zeroVector
      //        } else {
      //          val ratios = (w zip v).filter(_ != (0, 0))
      //          if (ratios.indexWhere({ p => p._2 == 0 }) < 0) {
      //            ratios.map({ p => modP.quotient(p._1, p._2) }).distinct.size == 1
      //          } else {
      //            false
      //          }
      //        }
      //      }
      //
      //    for (v <- normalizedEigenvectors; m0 <- classCoefficients; w = new Matrix(m0.size, m0).apply(v)(modP)) {
      //      require(proportional(w, v))
      //    }
      //
      //    require(new Matrix(conjugacyClasses.size, normalizedEigenvectors).rank() == conjugacyClasses.size)

      normalizedEigenvectors
    }

    def characterTableModPreferredPrime = {
      val k = conjugacyClasses.size
      implicit val modP = Mod(preferredPrime)

      def sqrt(x: Int) = {
        import net.tqft.toolkit.arithmetic.Mod._
        (0 to preferredPrime / 2).find({ n => ((n * n - x) mod preferredPrime) == 0 }).get
      }

      val omega = classCoefficientSimultaneousEigenvectorsModPrime
      val degrees = for (omega_i <- omega) yield {
        sqrt(modP.quotient(finiteGroup.size, (for (j <- 0 until k) yield modP.quotient(omega_i(j) * omega_i(exponentiationOnConjugacyClasses(-1)(j)), conjugacyClasses(j).size)).sum))
      }

      for (i <- 0 until k) yield for (j <- 0 until k) yield modP.quotient(omega(i)(j) * degrees(i), conjugacyClasses(j).size)
    }

    val k = conjugacyClasses.size

    implicit val rationals = Gadgets.Rationals
    implicit val modP = Mod(preferredPrime)

    val cyclotomicNumbers = NumberField.cyclotomic(exponent)(rationals)
    val zeta = Polynomial.identity(rationals)
    val chi = characterTableModPreferredPrime
    def order(n: Int) = (1 until preferredPrime).find({ m => modP.power(n, m) == 1 }).get
    val z = (1 until preferredPrime).find({ n => order(n) == exponent }).get

    val zpower = {
      import net.tqft.toolkit.functions.Memo._
      { k: Int => modP.power(z, k) }.memo
    }
    val zetapower = {
      import net.tqft.toolkit.functions.Memo._
      { k: Int => cyclotomicNumbers.power(zeta, k) }.memo
    }

    def mu(i: Int)(j: Int)(s: Int) = modP.quotient(modP.add(for (n <- 0 until exponent) yield modP.multiply(chi(i)(exponentiationOnConjugacyClasses(n)(j)), zpower(-s * n))), exponent)

    val unsortedCharacters = (for (i <- 0 until k par) yield (for (j <- 0 until k par) yield cyclotomicNumbers.add(for (s <- 0 until exponent) yield cyclotomicNumbers.multiplyByInt(zetapower(s), mu(i)(j)(s)))).seq).seq
    (exponent, unsortedCharacters.sortBy({ v => (v(0).constantTerm, v.tail.headOption.map({ p => rationals.negate(p.constantTerm) })) }))
  }

  lazy val tensorProductMultiplicities: Seq[Seq[Seq[Int]]] = {
    val k = conjugacyClasses.size
    val (p, chi) = characterTable
    import Implicits.Rationals
    implicit val Q = NumberField.cyclotomic(p)

    def pairing(x: Seq[Polynomial[Fraction[Int]]], y: Seq[Polynomial[Fraction[Int]]]) = {
      Q.quotientByInt(Q.add((x zip y zip conjugacyClasses.map(_.size)).map({ p => Q.multiplyByInt(Q.multiply(p._1._1, p._1._2), p._2) })), finiteGroup.size)
    }

    def lower(p: Polynomial[Fraction[Int]]) = {
      require(p == Q.zero || p.maximumDegree == Some(0))
      val c = p.constantTerm
      require(c.denominator == 1)
      c.numerator
    }

    (for (i <- 0 until k par) yield {
      (for (j <- 0 until k par) yield {
        val product = (chi(i) zip chi(j)).map({ p => Q.multiply(p._1, p._2) }) map (Q.bar _)
        for (c <- chi) yield lower(pairing(product, c))
      }).seq
    }).seq
  }
}

trait FinitelyGeneratedFiniteGroup[A] extends FiniteGroup[A] { fgFiniteGroup =>
  def generators: Set[A]
  override def unsortedConjugacyClasses = GroupActions.conjugationAction(fgFiniteGroup).orbits(generators, elements).toSeq
}

trait EquivalenceClass[A] extends Elements[A] {
  def representative: A
  def contains(a: A) = elements.contains(a)

  def leastRepresentative(implicit o: Ordering[A]) = elements.min

  override def equals(other: Any) = {
    other match {
      case other: EquivalenceClass[_] => {
        contains(other.asInstanceOf[EquivalenceClass[A]].representative)
      }
      case _ => false
    }
  }
  override def hashCode = {
    elements.hashCode
  }
}

object EquivalenceClass {
  implicit def equivalenceClassOrdering[A](implicit o: Ordering[A]): Ordering[EquivalenceClass[A]] = new Ordering[EquivalenceClass[A]] {
    def compare(x: EquivalenceClass[A], y: EquivalenceClass[A]) = o.compare(x.leastRepresentative, y.leastRepresentative)
  }
}

trait FiniteGroupHomomorphism[A, B] extends Homomorphism[FiniteGroup, A, B] { homomorphism =>
  def kernel = source.subgroup(source.elements.par.filter(homomorphism(_) == target.one).seq)
}

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
      def one = new C(_group.one)
      def inverse(a: LeftCoset[A]) = new C(_group.inverse(a.representative))
      def multiply(a: LeftCoset[A], b: LeftCoset[A]) = new C(_group.multiply(a.representative, b.representative))

      def elements = leftCosets(_group, normalSubgroup)

      def ordering = ???
    }
  }

  private class TrivialGroup[A](identity: A) extends FiniteGroup[A] {
    def one = identity
    def inverse(a: A) = identity
    def multiply(a: A, b: A) = identity

    val elements = Set(identity)

    def ordering = ???
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

    def ordering = {
      import net.tqft.toolkit.collections.LexicographicOrdering._
      implicitly[Ordering[(Int, Boolean)]]
    }
  }
  private class CyclicGroup(n: Int) extends FiniteGroup[Int] {
    import net.tqft.toolkit.arithmetic.Mod._

    override def one = 0
    override def inverse(a: Int) = -a mod n
    override def multiply(a: Int, b: Int) = (a + b) mod n

    override val elements = (0 until n).toSet

    def ordering = implicitly[Ordering[Int]]
  }
  private class PermutationGroup(n: Int) extends FiniteGroup[Seq[Int]] {
    import net.tqft.toolkit.permutations.Permutations
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation
    def elements = Permutations.of(n).toSet
    def inverse(x: Seq[Int]) = Permutations.inverse(x)
    def multiply(x: Seq[Int], y: Seq[Int]) = x permute y
    def one = 0 until n

    def ordering = {
      import net.tqft.toolkit.collections.LexicographicOrdering._
      implicitly[Ordering[Seq[Int]]]
    }
  }

  def symmetricGroup(n: Int): FiniteGroup[Seq[Int]] = new PermutationGroup(n)

  def signature(n: Int): FiniteGroupHomomorphism[Seq[Int], Int] = new FiniteGroupHomomorphism[Seq[Int], Int] {
    val source = symmetricGroup(n)
    val target = cyclicGroup(2)
    def apply(p: Seq[Int]) = {
      var k = 0
      for (i <- 0 until p.size; j <- 0 until i; if p(i) < p(j)) k = k + 1
      k % 2
    }
  }

  def alternatingGroup(n: Int): FiniteGroup[Seq[Int]] = signature(n).kernel
  def signedPermutationGroup(n: Int): FiniteGroup[(Seq[Int], Seq[Int])] = semidirectProduct(symmetricGroup(n), power(cyclicGroup(2), n), GroupActions.permutationAction[Int])

  lazy val Mathieu11 = symmetricGroup(11).subgroupGeneratedBy(Set(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0),
    Seq(0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7)))
  lazy val Mathieu12 = symmetricGroup(12).subgroupGeneratedBy(Set(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11),
    Seq(0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7, 11),
    Seq(11, 10, 5, 7, 8, 2, 9, 3, 4, 6, 1, 0)))
  lazy val Mathieu22 = symmetricGroup(22).subgroupGeneratedBy(Set(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 11),
    Seq(3, 7, 0, 4, 8, 1, 5, 9, 2, 6, 10, 14, 18, 11, 15, 19, 12, 16, 20, 13, 17, 21),
    Seq(20, 9, 12, 16, 18, 1, 6, 5, 17, 7, 21, 13, 3, 15, 14, 19, 2, 4, 8, 11, 0, 10)))
  lazy val Mathieu23 = symmetricGroup(23).subgroupGeneratedBy(Set(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 0),
    Seq(0, 1, 16, 12, 3, 5, 8, 17, 2, 6, 11, 22, 13, 18, 19, 14, 9, 10, 4, 21, 15, 20, 7)))
  lazy val Mathieu24 = symmetricGroup(24).subgroupGeneratedBy(Set(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 0, 23),
    Seq(0, 1, 16, 12, 3, 5, 8, 17, 2, 6, 11, 22, 13, 18, 19, 14, 9, 10, 4, 21, 15, 20, 7, 23),
    Seq(23, 22, 11, 15, 17, 9, 19, 13, 20, 5, 16, 2, 21, 7, 18, 3, 10, 4, 14, 6, 8, 12, 1, 0)))

  def semidirectProduct[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B], action: GroupAction[A, B]): FiniteGroup[(A, B)] = {
    new FiniteGroup[(A, B)] {
      def elements = for (g1 <- group1.elements; g2 <- group2.elements) yield (g1, g2)
      def one = (group1.one, group2.one)
      def inverse(x: (A, B)) = ???
      def multiply(x: (A, B), y: (A, B)) = ???

      def ordering = {
        import net.tqft.toolkit.collections.LexicographicOrdering._
        implicit val ordering1 = group1.ordering
        implicit val ordering2 = group2.ordering
        implicitly[Ordering[(A, B)]]
      }

    }
  }

  def product[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B]) = semidirectProduct(group1, group2, GroupActions.trivialAction)

  def power[A](group: FiniteGroup[A], k: Int): FiniteGroup[Seq[A]] = {
    new FiniteGroup[Seq[A]] {
      def elements = ???
      def one = List.fill(k)(group.one)
      def inverse(x: Seq[A]) = x.map(group.inverse _)
      def multiply(x: Seq[A], y: Seq[A]) = (x zip y).map({ case (xg, yg) => group.multiply(xg, yg) })
      def ordering = {
        import net.tqft.toolkit.collections.LexicographicOrdering._
        implicit val orderingA = group.ordering
        implicitly[Ordering[Seq[A]]]
      }
    }
  }
}

trait GroupAction[A, B] {
  def act(a: A, b: B): B
  def orbits(generators: Set[A], objects: Set[B])(implicit ordering: Ordering[B]): Set[Orbit[A, B]] = {
    class O(val representative: B) extends Orbit[A, B] {
      override def stabilizer = ???
      override lazy val elements = extendElements(HashSet.empty[B], Set(representative).par)

      @scala.annotation.tailrec
      private def extendElements(elements: Set[B], newestElements: GenSet[B]): Set[B] = {
        if (newestElements.isEmpty) {
          elements
        } else {
          val allElements = elements union newestElements;
          extendElements(allElements, (for (b <- newestElements; a <- generators) yield act(a, b)) diff allElements)
        }
      }
    }

    def extractOrbits(objects: Set[B], orbits: Set[Orbit[A, B]]): Set[Orbit[A, B]] = {
      if (objects.isEmpty) {
        orbits
      } else {
        val newOrbit = new O(objects.head)
        extractOrbits(objects diff newOrbit.elements, orbits + newOrbit)
      }
    }

    extractOrbits(objects, Set())
  }
}

trait Orbit[A, B] extends EquivalenceClass[B] {
  def stabilizer: FiniteGroup[A]
}

object GroupActions {
  def trivialAction[A, B]: GroupAction[A, B] = new GroupAction[A, B] {
    def act(a: A, b: B) = b
  }
  def permutationAction[C]: GroupAction[Seq[Int], Seq[C]] = new GroupAction[Seq[Int], Seq[C]] {
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation

    def act(a: Seq[Int], b: Seq[C]) = a permute b
  }
  def conjugationAction[A](group: Group[A]) = new GroupAction[A, A] {
    def act(a: A, b: A) = group.multiply(group.inverse(a), b, a)
  }
}
